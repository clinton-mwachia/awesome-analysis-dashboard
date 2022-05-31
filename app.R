library(shiny)
library(dplyr)
library(semantic.dashboard)

data_page <- (
  fluidRow(
    box(
      title = "Filters",width = 5, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "green",
      selectInput("one","input one", choices = mtcars$mpg),
      selectInput("two","input two", choices = c("a","b"))
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
  output$table <- renderTable({
    mtcars %>%
      filter(mpg == input$one)
  })
}

shinyApp(ui, server)