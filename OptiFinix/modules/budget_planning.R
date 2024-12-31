# Module for Budget Planning

# UI for Budget Planning
budgetPlanningUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    titlePanel("Budget Planning"),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          selectInput(ns("planning_year"), "Select Year:", choices = 2023:2030, selected = 2023),
          textInput(ns("new_category"), "New Category Name:", ""),
          selectInput(ns("table_selection"), "Select Table:", 
                      choices = c("Income", "Expenses", "Savings")),
          actionButton(ns("add_category"), "Add Category"),
          selectInput(ns("remove_category"), "Select Category to Remove:", choices = NULL),
          actionButton(ns("remove_category_btn"), "Remove Selected Category"),
          actionButton(ns("save_planning"), "Save Forecasts")
        )
      ),
      column(
        width = 9,
        tabsetPanel(
          tabPanel(
            "Income",
            h4("Forecast Income"),
            DTOutput(ns("income_table"))
          ),
          tabPanel(
            "Expenses",
            h4("Forecast Expenses"),
            DTOutput(ns("expenses_table"))
          ),
          tabPanel(
            "Savings",
            h4("Forecast Savings"),
            DTOutput(ns("savings_table"))
          )
        )
      )
    )
  )
}

# Server for Budget Planning
budgetPlanningServer <- function(input, output, session) {
  ns <- session$ns
  
  # Initialize data for income, expenses, and savings
  initialize_table <- function(categories) {
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    data <- data.frame(Category = categories, stringsAsFactors = FALSE)
    for (month in months) {
      data[[month]] <- 0
    }
    data
  }
  
  # Define categories (excluding Total)
  income_categories <- c("Deposit Other", "Deposit ONE", "Deposit TWO", "<Account Adjustment>", "Transfer from Savings", "Transfer from Other")
  expense_categories <- c("Apple Music", "Cable", "Car Payment", "Cell", "Chegg", "Church", "Credit Card", "Dining Out", "Education/Training", "Electric", "Fitness", "Gas & Misc.", "Groceries", "Gifts", "Haircuts", "Health Care", "Heat", "Internet", "Other", "Recreation", "Rent", "Student Loans", "Utilities", "Water & Waste")
  savings_categories <- c("Wells Fargo Savings", "Capital One")
  
  # Reactive values for each budget table
  income_data <- reactiveVal(initialize_table(income_categories))
  expenses_data <- reactiveVal(initialize_table(expense_categories))
  savings_data <- reactiveVal(initialize_table(savings_categories))
  
  # Function to append a total row
  append_total_row <- function(data) {
    numeric_data <- data[, -1]  # Exclude the 'Category' column
    numeric_data[] <- lapply(numeric_data, as.numeric)  # Ensure all columns are numeric
    total_row <- colSums(numeric_data, na.rm = TRUE)  # Sum all numeric columns
    total_row <- as.data.frame(t(total_row))
    total_row <- cbind(Category = "Total", total_row)
    rbind(data, total_row)
  }
  
  # Populate the remove_category dropdown dynamically
  observe({
    table_type <- input$table_selection
    if (table_type == "Income") {
      current_data <- income_data()
    } else if (table_type == "Expenses") {
      current_data <- expenses_data()
    } else {
      current_data <- savings_data()
    }
    # Exclude "Total" from the dropdown
    categories <- current_data$Category[current_data$Category != "Total"]
    updateSelectInput(session, "remove_category", choices = categories)
  })
  
  # Remove the selected category
  observeEvent(input$remove_category_btn, {
    selected_category <- input$remove_category
    if (is.null(selected_category) || selected_category == "") {
      showNotification("Please select a category to remove.", type = "error")
      return()
    }
    
    table_type <- input$table_selection
    if (table_type == "Income") {
      data <- income_data()
      data <- data[data$Category != selected_category, ]  # Remove the selected category
      income_data(data)  # Update the reactive value
      # Save the updated data to file
      year <- as.character(input$planning_year)
      base_dir <- file.path("data", year, "budget")
      write.csv(data, file = file.path(base_dir, "income_data.csv"), row.names = FALSE)
    } else if (table_type == "Expenses") {
      data <- expenses_data()
      data <- data[data$Category != selected_category, ]
      expenses_data(data)
      # Save the updated data to file
      year <- as.character(input$planning_year)
      base_dir <- file.path("data", year, "budget")
      write.csv(data, file = file.path(base_dir, "expenses_data.csv"), row.names = FALSE)
    } else if (table_type == "Savings") {
      data <- savings_data()
      data <- data[data$Category != selected_category, ]
      savings_data(data)
      # Save the updated data to file
      year <- as.character(input$planning_year)
      base_dir <- file.path("data", year, "budget")
      write.csv(data, file = file.path(base_dir, "savings_data.csv"), row.names = FALSE)
    }
    showNotification(paste("Category", selected_category, "removed successfully!"), type = "message")
  })
  
  # Load or initialize data based on the selected year
  observeEvent(input$planning_year, {
    year <- as.character(input$planning_year)
    base_dir <- file.path("data", year, "budget")
    
    # File paths
    income_file <- file.path(base_dir, "income_data.csv")
    expenses_file <- file.path(base_dir, "expenses_data.csv")
    savings_file <- file.path(base_dir, "savings_data.csv")
    
    # Load or initialize
    if (file.exists(income_file)) {
      income_data(read.csv(income_file, stringsAsFactors = FALSE))
    } else {
      income_data(initialize_table(income_categories))
    }
    
    if (file.exists(expenses_file)) {
      expenses_data(read.csv(expenses_file, stringsAsFactors = FALSE))
    } else {
      expenses_data(initialize_table(expense_categories))
    }
    
    if (file.exists(savings_file)) {
      savings_data(read.csv(savings_file, stringsAsFactors = FALSE))
    } else {
      savings_data(initialize_table(savings_categories))
    }
  })
  
  # Render tables with total row appended
  output$income_table <- renderDT({
    datatable(
      append_total_row(income_data()),
      editable = TRUE,
      options = list(pageLength = -1, paging = FALSE) # Show all rows
    )
  })
  
  output$expenses_table <- renderDT({
    datatable(
      append_total_row(expenses_data()),
      editable = TRUE,
      options = list(pageLength = -1, paging = FALSE) # Show all rows
    )
  })
  
  output$savings_table <- renderDT({
    datatable(
      append_total_row(savings_data()),
      editable = TRUE,
      options = list(pageLength = -1, paging = FALSE) # Show all rows
    )
  })
  
  # Handle edits
  observeEvent(input$income_table_cell_edit, {
    info <- input$income_table_cell_edit
    updated_data <- income_data()
    col_name <- colnames(updated_data)[info$col]
    updated_data[info$row, col_name] <- info$value
    income_data(updated_data)
  })
  
  observeEvent(input$expenses_table_cell_edit, {
    info <- input$expenses_table_cell_edit
    updated_data <- expenses_data()
    col_name <- colnames(updated_data)[info$col]
    updated_data[info$row, col_name] <- info$value
    expenses_data(updated_data)
  })
  
  observeEvent(input$savings_table_cell_edit, {
    info <- input$savings_table_cell_edit
    updated_data <- savings_data()
    col_name <- colnames(updated_data)[info$col]
    updated_data[info$row, col_name] <- info$value
    savings_data(updated_data)
  })
  
  # Add new category
  observeEvent(input$add_category, {
    new_category <- input$new_category
    if (new_category == "") {
      showNotification("Please enter a category name.", type = "error")
      return()
    }
    
    table_type <- input$table_selection
    if (table_type == "Income") {
      data <- income_data()
      if (new_category %in% data$Category) {
        showNotification("Category already exists in Income table.", type = "warning")
        return()
      }
      # Add the new category above the Total row
      data <- rbind(
        data[1:(nrow(data) - 1), ],  # All rows except the Total row
        c(new_category, rep(0, ncol(data) - 1)),  # New category row
        data[nrow(data), ]  # Re-add the Total row
      )
      income_data(data)
    } else if (table_type == "Expenses") {
      data <- expenses_data()
      if (new_category %in% data$Category) {
        showNotification("Category already exists in Expenses table.", type = "warning")
        return()
      }
      # Add the new category above the Total row
      data <- rbind(
        data[1:(nrow(data) - 1), ],  # All rows except the Total row
        c(new_category, rep(0, ncol(data) - 1)),  # New category row
        data[nrow(data), ]  # Re-add the Total row
      )
      expenses_data(data)
    } else if (table_type == "Savings") {
      data <- savings_data()
      if (new_category %in% data$Category) {
        showNotification("Category already exists in Savings table.", type = "warning")
        return()
      }
      # Add the new category above the Total row
      data <- rbind(
        data[1:(nrow(data) - 1), ],  # All rows except the Total row
        c(new_category, rep(0, ncol(data) - 1)),  # New category row
        data[nrow(data), ]  # Re-add the Total row
      )
      savings_data(data)
    }
    showNotification(paste("Added new category to", table_type, "table:"), type = "message")
  })
  
  # Save data
  observeEvent(input$save_planning, {
    year <- as.character(input$planning_year)
    base_dir <- file.path("data", year, "budget")
    if (!dir.exists(base_dir)) {
      dir.create(base_dir, recursive = TRUE)
    }
    
    write.csv(income_data(), file = file.path(base_dir, "income_data.csv"), row.names = FALSE)
    write.csv(expenses_data(), file = file.path(base_dir, "expenses_data.csv"), row.names = FALSE)
    write.csv(savings_data(), file = file.path(base_dir, "savings_data.csv"), row.names = FALSE)
    showNotification(paste("Forecasts saved successfully for year", year, "!"), type = "message")
  })
}
