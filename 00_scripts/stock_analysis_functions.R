get_stock_list <-
function(stock_index = "DOW") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker symbol", "Company")),
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}
get_symbol_from_user_input <-
function(user_input) {
  user_input %>%
    str_split(", ") %>%  # Split the string at the comma followed by a space
    pluck(1, 1)          # Extract the first element of the first list
}
get_stock_data <-
function(stock_symbol, 
                           from = today() - days(180), 
                           to   = today(), 
                           mavg_short = 20, mavg_long = 50) {
  
  stock_data <- stock_symbol %>% 
    # Retrieve market data
    quantmod::getSymbols(
      src = "yahoo", 
      from = from, 
      to = to, 
      auto.assign = FALSE) %>% 
    
    # Convert to tibble
    timetk::tk_tbl(preserve_index = TRUE, silent = TRUE) %>% 
    
    # Add currency column (based on symbol)
    mutate(currency = case_when(
      str_detect(stock_symbol, "\\.DE") ~ "EUR",
      TRUE ~ "USD")) %>% 
    
    # Modify tibble
    set_names(c("date", "open", "high", "low", "close", "volume", "adjusted", "currency")) %>% 
    drop_na() %>%
    
    # Convert the date column to a date object
    mutate(date = lubridate::ymd(date)) %>% 
    
    # Add the moving averages
    mutate(mavg_short = rollmean(adjusted, k = mavg_short, fill = NA, align = "right")) %>% 
    mutate(mavg_long = rollmean(adjusted, k = mavg_long, fill = NA, align = "right")) %>% 
    
    # Select the date and the adjusted column
    dplyr::select(date, adjusted, mavg_short, mavg_long, currency)
  
  return(stock_data)
}
plot_stock_data <-
function(stock_data) {
  
  # Convert to long format
  stock_data_long <- stock_data %>%
    pivot_longer(cols = c(adjusted, mavg_short, mavg_long),
                 names_to = "legend", 
                 values_to = "value", 
                 names_ptypes = list(legend = factor()))
  
  # Create ggplot
  g <- stock_data_long %>%
    ggplot(aes(x = date, y = value, color = legend, group = legend)) +
    geom_line(aes(linetype = legend)) +
    labs(y = "Adjusted Share Price", x = "") +
    scale_y_continuous(labels = currency_format(stock_data %>% pull(currency) %>% first())) +
    theme_minimal() + 
    theme(legend.title = element_blank())
  
  # Convert to interactive plotly plot
  ggplotly(g)
}
currency_format <-
function(currency) {
  if (currency == "USD") {
    x <- scales::dollar_format(largest_with_cents = 10)
  } else if (currency == "EUR") {
    x <- scales::dollar_format(prefix = "", suffix = " €", 
                               big.mark = ".", decimal.mark = ",",
                               largest_with_cents = 10)
  }
  return(x)
}
generate_commentary <-
function(data, user_input) {
  
  # Generate the warning signal based on the last value of the moving averages
  warning_signal <- data %>%
    tail(1) %>%
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
    pull(mavg_warning_flag)
  
  # Calculate the moving average periods based on the number of NAs
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  # Generate commentary based on the warning signal
  if (warning_signal) {
    commentary <- str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends.")
  } else {
    commentary <- str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends.")
  }
  
  return(commentary)
}
