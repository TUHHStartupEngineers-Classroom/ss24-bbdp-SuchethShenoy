# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary

# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(rsconnect)

library(plotly)
library(tidyverse)

library(rvest)
library(lubridate)
library(quantmod)
library(glue)

source("00_scripts/stock_analysis_functions.R")

# UI ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 1.0 HEADER ----
  headerPanel("Stock Analyzer App"),
  
  # 2.0 APPLICATION UI -----
  sidebarLayout(
    sidebarPanel(
      # Index Selection Dropdown
      pickerInput(
        inputId = "index_symbol",
        label = "Select Index",
        choices = c("DOW", "SP500", "NASDAQ"),
        selected = "DOW"
      ),
      
      # Stock List Dropdown (populated dynamically based on selected index)
      uiOutput("stock_symbol_dropdown"),
      
      # Analyze Button with Font Awesome Icon
      actionButton(inputId = "analyze", label = "Analyze", icon = icon("chart-line")),
      
      # Horizontal rule
      hr(),
      
      # Date Range Input
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range",
        start = Sys.Date() - 180,  # Default to 180 days ago
        end = Sys.Date(),          # Default to today
        format = "yyyy-mm-dd"
      ),
      
      # Horizontal rule
      hr(),
      
      # Short Moving Average Slider
      sliderInput(
        inputId = "mavg_short",
        label = "Short Moving Average (MAVG)",
        value = 20,
        min = 5,
        max = 40,
        step = 1
      ),
      
      # Long Moving Average Slider
      sliderInput(
        inputId = "mavg_long",
        label = "Long Moving Average (MAVG)",
        value = 50,
        min = 50,
        max = 120,
        step = 1
      )
      
    ),
    
    mainPanel(
      # Title for Plot
      fluidRow(
        div(h4("Interactive Time Series Plot"), width = 12)
      ),
      
      # Interactive Plot
      fluidRow(
        div(
          textOutput("plot_header"),
          plotlyOutput(outputId = "interactive_plot")
        ),
        width = 12
      ),
      
      # Analyst Commentary
      fluidRow(
        div(
          h4("Analyst Commentary"),
          textOutput("commentary_output")
        ),
        width = 12
      )
    )
  )
)


# SERVER ----
server <- function(input, output, session) {
  
  # Reactive function to extract selected stock symbol based on index
  stock_list_tbl <- reactive({
    get_stock_list(input$index_symbol)
  })
  
  # Render stock symbol dropdown based on selected index
  output$stock_symbol_dropdown <- renderUI({
    pickerInput(
      inputId = "stock_symbol",
      label = "Select Stock",
      choices = stock_list_tbl()$label,
      multiple = FALSE,
      options = pickerOptions(
        actionsBox = FALSE,
        liveSearch = TRUE,
        size = 10
      )
    )
  })
  
  # Reactive function to extract selected stock symbol
  stock_symbol <- eventReactive(input$analyze, {
    input$stock_symbol
  }, ignoreNULL = FALSE)
  
  # Render selected symbol as plot header in the UI
  output$plot_header <- renderText({
    stock_symbol()
  })
  
  # Reactive expression to update stock data based on selected symbol, sliders, and date range
  stock_data_tbl <- reactive({
    req(input$date_range)  # Ensure date range input is available
    
    symbol <- stock_symbol()
    
    data <- tryCatch({
      symbol %>%
        get_symbol_from_user_input() %>%
        get_stock_data(
          from = input$date_range[1], 
          to = input$date_range[2],
          mavg_short = input$mavg_short,
          mavg_long = input$mavg_long
        )
    }, error = function(e) {
      NULL  # Return NULL in case of error
    })
    
    data
  })
  
  # Output to display stock data table
  output$stock_data <- renderPrint({
    stock_data_tbl()
  })
  
  # Output to render interactive plot
  output$interactive_plot <- renderPlotly({
    req(stock_data_tbl())  # Ensure stock_data_tbl is available
    plot_stock_data(stock_data_tbl())
  })
  
  # Output to render commentary
  output$commentary_output <- renderText({
    req(stock_data_tbl())  # Ensure stock_data_tbl is available
    generate_commentary(stock_data_tbl(), user_input = stock_symbol())
  })
  
  # Update stock data when date range changes
  observeEvent(input$date_range, {
    stock_data_tbl()  # Trigger reactive update
  })
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)