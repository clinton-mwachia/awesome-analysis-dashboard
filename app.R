library(shiny)
library(semantic.dashboard)

data_page <- (
  fluidRow(
    box(
      title = "Filters",width = 5, collapsible=FALSE,
      ribbon = FALSE,
      h3("data filters")
    ),
    box(
      title = "Output", width = 11, collapsible=FALSE,
      ribbon = FALSE,
      h3("Data output")
    )
  )
)

analysis_page <- (
  box(
    title = "Analysis", width = 16, collapsible=FALSE,
    ribbon = FALSE,
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
}

shinyApp(ui, server)