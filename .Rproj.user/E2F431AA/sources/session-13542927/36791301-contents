library(shiny)
library(ggplot2)
library(reshape2)

# Define server logic for monthly expense calculator and plot
shinyServer(function(input, output, session) {
  
  # Reactive value to store detailed expenses over time
  expenseData <- reactiveVal(data.frame(Month = factor(), Rent = numeric(), Utilities = numeric(), 
                                        Groceries = numeric(), Transportation = numeric(), 
                                        Entertainment = numeric(), Miscellaneous = numeric(), 
                                        Total = numeric(), stringsAsFactors = FALSE))
  
  # Reactive expression to calculate total expenses
  totalExpenses <- reactive({
    input$calculate
    isolate({
      total <- input$rent + input$utilities + input$groceries +
        input$transportation + input$entertainment + input$miscellaneous
      return(total)
    })
  })
  
  # Observe calculate button to update expense data
  observeEvent(input$calculate, {
    currentData <- expenseData()
    newEntry <- data.frame(Month = factor(input$month, levels = month.name), 
                           Rent = input$rent, Utilities = input$utilities, 
                           Groceries = input$groceries, Transportation = input$transportation, 
                           Entertainment = input$entertainment, Miscellaneous = input$miscellaneous, 
                           Total = totalExpenses())
    updatedData <- rbind(currentData, newEntry)
    updatedData <- updatedData[order(match(updatedData$Month, month.name)), ]
    expenseData(updatedData)
  })
  
  # Observe reset button to clear expense data
  observeEvent(input$reset, {
    expenseData(data.frame(Month = factor(), Rent = numeric(), Utilities = numeric(), 
                           Groceries = numeric(), Transportation = numeric(), 
                           Entertainment = numeric(), Miscellaneous = numeric(), 
                           Total = numeric(), stringsAsFactors = FALSE))
  })
  
  # Render the table of expenses
  output$expenseTable <- renderTable({
    input$calculate
    isolate({
      data.frame(
        Category = c("Rent", "Utilities", "Groceries", "Transportation", "Entertainment", "Miscellaneous"),
        Amount = c(input$rent, input$utilities, input$groceries, 
                   input$transportation, input$entertainment, input$miscellaneous)
      )
    })
  })
  
  # Render the total expenses
  output$totalExpenses <- renderText({
    totalExpenses()
  })
  
  # Render the breakdown table by month
  output$breakdownTable <- renderTable({
    expenseData()
  })
  
  # Render the plot of total expenses over time
  output$expensePlot <- renderPlot({
    meltedData <- melt(expenseData(), id.vars = "Month", measure.vars = c("Rent", "Utilities", 
                                                                          "Groceries", "Transportation", 
                                                                          "Entertainment", "Miscellaneous"))
    ggplot(meltedData, aes(x = Month, y = value, fill = variable)) +
      geom_col(position = "dodge") +
      labs(title = "Monthly Expense Breakdown", x = "Month", y = "Expense Amount") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
  })
})
