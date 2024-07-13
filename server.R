# Load necessary libraries
library(shiny)
library(quantmod)
library(forecast)
library(ggplot2)
library(DT)
library(httr)
library(jsonlite)
library(plotly)

# Your Alpha Vantage API key
api_key <- "45S20HU801NTP9Q2"

# Function to fetch financial data from Alpha Vantage
get_financial_data <- function(symbol) {
  url <- paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=", symbol, "&apikey=", api_key)
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    return(content)
  } else {
    return(NULL)
  }
}

# Function to calculate moving averages
calculate_emas <- function(data) {
  data$EMA_50 <- EMA(Cl(data), n = 50)
  data$EMA_100 <- EMA(Cl(data), n = 100)
  data$EMA_200 <- EMA(Cl(data), n = 200)
  data
}

server <- function(input, output, session) {
  
  stockData <- reactive({
    req(input$update)
    isolate({
      getSymbols(input$stock, src = "yahoo", auto.assign = FALSE)
    })
  })
  
  financialData <- reactive({
    get_financial_data(input$stock)
  })
  
  output$stockPlot <- renderPlot({
    req(stockData())
    data <- stockData()
    
    # Prepare data for OHLC plot
    ohlc_data <- data.frame(
      Date = index(data),
      Open = as.numeric(Op(data)),
      High = as.numeric(Hi(data)),
      Low = as.numeric(Lo(data)),
      Close = as.numeric(Cl(data))
    )
    
    ggplot(ohlc_data, aes(x = Date)) +
      geom_segment(aes(y = Low, yend = High, xend = Date), color = "black") +
      geom_segment(aes(y = Open, yend = Open, xend = Date), color = "blue") +
      geom_segment(aes(y = Close, yend = Close, xend = Date), color = "red") +
      labs(title = paste("OHLC Chart for", input$stock), x = "Date", y = "Price") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  output$dynamicPlot <- renderPlotly({
    req(stockData())
    data <- stockData()
    data <- calculate_emas(data)  # Calculate EMAs
    
    # Convert xts object to data frame
    df <- data.frame(
      Date = index(data),
      Close = as.numeric(Cl(data)),
      EMA_50 = as.numeric(data$EMA_50),
      EMA_100 = as.numeric(data$EMA_100),
      EMA_200 = as.numeric(data$EMA_200)
    )
    
    plot_ly(data = df, x = ~Date) %>%
      add_lines(y = ~Close, name = 'Price', line = list(color = 'blue', width = 2)) %>%
      add_lines(y = ~EMA_50, name = 'EMA 50', line = list(color = 'green', dash = 'dash')) %>%
      add_lines(y = ~EMA_100, name = 'EMA 100', line = list(color = 'orange', dash = 'dot')) %>%
      add_lines(y = ~EMA_200, name = 'EMA 200', line = list(color = 'red', dash = 'dashdot')) %>%
      layout(
        title = list(
          text = paste("Technical Analysis for", input$stock),
          font = list(size = 20),
          x = 0.5,  # Centering the title
          xanchor = "center"
        ),
        xaxis = list(title = "Date", rangeslider = list(visible = TRUE), type = "date"),
        yaxis = list(title = "Price"),
        hovermode = "x unified",
        legend = list(orientation = 'h', x = 0.1, y = -0.3)
      )
  })
  
  output$advice <- renderText({
    data <- stockData()
    data <- calculate_emas(data)  # Calculate EMAs
    price <- tail(Cl(data), 1)
    ema_50 <- tail(data$EMA_50, 1)
    ema_100 <- tail(data$EMA_100, 1)
    
    if (price > ema_50) {
      support_resistance <- "above"
    } else {
      support_resistance <- "below"
    }
    
    if (ema_50 > ema_100) {
      golden_cross <- "Golden cross suggests uptrend or bullishness in the stock."
    } else {
      golden_cross <- ""
    }
    
    paste(
      "Current price:", price,
      "\n50 EMA:", ema_50,
      "\n100 EMA:", ema_100,
      "\nPrice is", support_resistance, "50 EMA.",
      "\n", golden_cross
    )
  })
  
  output$financialTable <- renderDT({
    financials <- financialData()
    
    # Print the financials to debug and see what data is returned
    print(financials)
    
    if (!is.null(financials)) {
      # Ensure all metrics have values, default to "N/A" if missing
      data <- data.frame(
        Metric = c("Earnings Per Share (EPS)", "Revenue Growth", "Profit Margin", "Price-to-Earnings (P/E) Ratio", 
                   "Price-to-Sales (P/S) Ratio", "Price-to-Book (P/B) Ratio", "Debt-to-Equity Ratio", "Interest Coverage Ratio"),
        Value = c(
          ifelse(!is.null(financials$EPS), financials$EPS, "N/A"),
          ifelse(!is.null(financials$RevenueGrowthTTM), paste0(financials$RevenueGrowthTTM, "%"), "N/A"),
          ifelse(!is.null(financials$ProfitMargin), paste0(financials$ProfitMargin, "%"), "N/A"),
          ifelse(!is.null(financials$PERatio), financials$PERatio, "N/A"),
          ifelse(!is.null(financials$PriceToSalesRatioTTM), financials$PriceToSalesRatioTTM, "N/A"),
          ifelse(!is.null(financials$PriceToBookRatio), financials$PriceToBookRatio, "N/A"),
          ifelse(!is.null(financials$DebtToEquity), financials$DebtToEquity, "N/A"),
          ifelse(!is.null(financials$InterestCoverage), financials$InterestCoverage, "N/A")
        )
      )
    } else {
      data <- data.frame(
        Metric = c("Earnings Per Share (EPS)", "Revenue Growth", "Profit Margin", "Price-to-Earnings (P/E) Ratio", 
                   "Price-to-Sales (P/S) Ratio", "Price-to-Book (P/B) Ratio", "Debt-to-Equity Ratio", "Interest Coverage Ratio"),
        Value = rep("N/A", 8)
      )
    }
    datatable(data, options = list(pageLength = 8, autoWidth = TRUE))
  })
}
