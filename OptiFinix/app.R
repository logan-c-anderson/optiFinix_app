library(shiny)
library(DT)

# Source the modules
source("modules/budget_planning.R")
source("modules/budget_tracking.R")

# Define the UI
ui <- fluidPage(
  # Link to external CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("OptiFinix - Budget Planner and Tracker"),
  
  tabsetPanel(
    tabPanel("Budget Planning", budgetPlanningUI("budget_planning")),
    tabPanel("Budget Tracking", budgetTrackingUI("budget_tracking"))
  )
)

# Define the server
server <- function(input, output, session) {
  callModule(budgetPlanningServer, "budget_planning")
  callModule(budgetTrackingServer, "budget_tracking")
}

# Run the app
shinyApp(ui = ui, server = server)
