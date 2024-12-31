# Module for Budget Tracking

budgetTrackingUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Budget Tracking"),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          dateInput(ns("tracking_date"), "Date:", value = Sys.Date()),
          textInput(ns("tracking_code"), "Code (e.g., POS, DEP):"),
          selectInput(ns("tracking_type"), "Type:", choices = c("Income", "Expense", "Savings")),
          selectInput(ns("tracking_category"), "Category:", choices = NULL),
          textInput(ns("tracking_description"), "Description:"),
          numericInput(ns("tracking_amount"), "Amount:", value = 0, min = 0),
          checkboxInput(ns("tracking_verified"), "Verified", value = FALSE),
          actionButton(ns("add_transaction"), "Add Transaction")
        )
      ),
      column(
        width = 9,
        h4("Track Transactions"),
        DTOutput(ns("tracking_table"))
      )
    )
  )
}

# Server for Budget Tracking
budgetTrackingServer <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive data for transactions
  tracking_data <- reactiveVal(data.frame(
    Date = as.Date(character()),
    Code = character(),
    Type = character(),
    Category = character(),
    Description = character(),
    Amount = numeric(),
    Verified = logical(),
    stringsAsFactors = FALSE
  ))
  
  # Add Transaction to Tracking Table
  observeEvent(input$add_transaction, {
    new_entry <- data.frame(
      Date = input$tracking_date,
      Code = input$tracking_code,
      Type = input$tracking_type,
      Category = input$tracking_category,
      Description = input$tracking_description,
      Amount = input$tracking_amount,
      Verified = input$tracking_verified,
      stringsAsFactors = FALSE
    )
    tracking_data(rbind(tracking_data(), new_entry))
  })
  
  # Render Tracking Table
  output$tracking_table <- renderDT({
    datatable(tracking_data(), editable = TRUE, options = list(pageLength = -1, paging = FALSE))
  })
  
  # Handle edits to the Tracking Table
  observeEvent(input$tracking_table_cell_edit, {
    info <- input$tracking_table_cell_edit
    data <- tracking_data()
    col_name <- colnames(data)[info$col]
    data[info$row, col_name] <- info$value
    tracking_data(data)
  })
  
  # Populate categories based on Budget Planning data
  observe({
    # For this to work, categories would need to be shared globally or passed in
    updateSelectInput(session, "tracking_category", choices = c("Income", "Expense", "Savings"))
  })
}
