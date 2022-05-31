# loading the libraries
library(shiny)
library(dplyr)
library(semantic.dashboard)


# loading the data
data <- mtcars

# data preparation
data <- data %>%
  mutate(
    vs = as.factor(vs),
    am = as.factor(am),
    gear = as.factor(gear),
    carb = as.factor(carb),
    cyl = as.factor(cyl)
  )

numeric_cols <- c("mpg", "disp", "hp", "drat" , "wt" , "qsec")
categoric_cols <- c("vs", "am", "gear", "carb", "cyl")

data_page <- (
  fluidRow(
    box(
      title = "Filters",width = 5, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "green",
      #selectInput("col1","column one", choices = numeric_cols),
      #selectInput("col2","column two", choices = numeric_cols)
      selectInput("vs", "select Engine", choices = unique(data$vs), 
                  selected = 0),
      selectInput("am", "select Transmission", choices = NULL),
      selectInput("gear", "select No. of gears", choices = unique(data$gear), 
                  selected = 1),
      selectInput("carb", "select No. of carburetors", choices = unique(data$vs), 
                  selected = 1)
    ),
    box(
      title = "Output", width = 11, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "teal",
      tableOutput("table")
    )
  )
)

analysis_page <- (
  box(
    title = "Analysis", width = 16, collapsible=FALSE,
    ribbon = FALSE, title_side = "top left",
    color = "olive",
    h3("Data Analysis")
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Awesome Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "Data", "data"),
      menuItem(tabName = "Analysis", "analysis")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Data",
        data_page
      ),
      tabItem(
        tabName = "Analysis",
        analysis_page
      )
    )
  )
)

server <- function(input, output, session) {
  home_data <- reactive({
    data %>%
      filter(
        vs == input$vs,
        am == input$am,
        gear == input$gear,
        carb == input$carb)
  })
  
  home_data_engine <- reactive({
    data %>%
      filter(vs == input$vs)
  })
  
  observeEvent(home_data_engine(),{
    updateSelectInput(session,"am", choices = home_data_engine()$am)
    updateSelectInput(session,"gear", choices = home_data_engine()$gear)
    updateSelectInput(session,"carb", choices = home_data_engine()$carb)
  })
  
  output$table <- renderTable({
    home_data()
  })
}

shinyApp(ui, server)