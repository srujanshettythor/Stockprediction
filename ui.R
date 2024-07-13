# Install and load shinythemes package if not already installed
if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}

# Load necessary libraries
library(shiny)
library(shinythemes)
library(plotly)
library(DT)

# Define the UI
shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  # Title panel
  titlePanel("Stock Market Prediction and Analysis"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Stock input without default value
      textInput("stock", "Enter Stock Symbol:"),
      
      # Plot type selection
      selectInput("plotType", "Select Plot Type:", 
                  choices = c("Line Chart", "Bar Chart", "Dynamic Chart")),
      
      # Update button
      actionButton("update", "Update", class = "btn-primary")
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Plot", plotOutput("stockPlot")),
        tabPanel("Dynamic Plot", plotlyOutput("dynamicPlot")),
        tabPanel("Financial Data",
                 dataTableOutput("financialTable"),
                 h4("Additional Insights"),
                 textOutput("profitConsistency"),
                 textOutput("shareHolding"),
                 textOutput("debtorsDays"),
                 textOutput("salesIncrease")
        ),
        tabPanel("Investment Advice", 
                 h4("Investment Advice"),
                 tags$div(style = "text-align:center;", textOutput("advice"))
        )
      )
    )
  )
))
