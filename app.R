library(shiny)
library(semantic.dashboard)

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
        h2("data")
      ),
      tabItem(
        tabName = "Analysis",
        h2("analysis")
      )
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)