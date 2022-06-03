# loading the libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(gapminder)
library(gganimate)
library(semantic.dashboard)

data("mtcars")

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

scatter_plot_page <- (
  fluidRow(
    box(
      title = "scatter plot with labels", width = 8, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "olive",
      plotOutput("sp1")
    ),
    box(
      title = "interactive plotly", width = 8, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "red",
      plotlyOutput("sp2")
    )
  )
)

scatter_plot_page1 <- (
  fluidRow(
    box(
      title = "scatter plot by groups", width = 8, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "yellow",
      plotOutput("spg")
    ),
    box(
      title = "scatter ploy by groups and size", width = 8, collapsible=FALSE,
      ribbon = FALSE, title_side = "top left",
      color = "green",
      plotOutput("spg2")
    )
  )
)

bar_plot_page <- (
    fluidRow(
      box(
        title = "scatter plot by groups", width = 8, collapsible=FALSE,
        ribbon = FALSE, title_side = "top left",
        color = "yellow",
        plotOutput("bp")
      ),
      box(
          title = "scatter ploy by groups and size", width = 8, collapsible=FALSE,
          ribbon = FALSE, title_side = "top left",
          color = "green",
          plotOutput("bp1")
          
      )
    )
)

box_plot <- (
  fluidRow(
    box(
      title = "box plot", width = 16,
      ribbon = FALSE, title_side = "top left",
      color = "yellow",
      plotOutput("bxp")
    )
  )
)

animation_plot <- (
  fluidRow(
    box(title = "Animation", color = "blue",width = 16),
    plotOutput("ap")
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Awesome Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "Data", "Data"),
      menuItem(tabName = "Analysis", "Scatter Plot"),
      menuItem(tabName = "BarPlot", "Bar Plot"),
      menuItem(tabName = "Animation", "Animation")
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
        scatter_plot_page,
        scatter_plot_page1
      ),
      tabItem(
        tabName = "BarPlot",
        bar_plot_page,
        box_plot
      ),
      tabItem(
        tabName = "Animation",
        animation_plot
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
        gear == input$gear)
  })
  
  home_data_engine <- reactive({
    data %>%
      filter(vs == input$vs)
  })
  
  observeEvent(home_data_engine(),{
    updateSelectInput(session,"am", choices = home_data_engine()$am)
    updateSelectInput(session,"gear", choices = home_data_engine()$gear)
  })
  
  output$table <- renderTable({
    home_data()
  })
  
  output$sp1 <- renderPlot({
    data <- data %>%
      head()
    
    data %>%
      head(30) %>%
      ggplot(aes(x=wt, y=mpg)) +
      geom_point() +
      geom_text( 
        label=rownames(data),
        nudge_x = 0.25, nudge_y = 0.25, 
        check_overlap = T
      )
  })
  
  output$sp2 <- renderPlotly({
    data <- data %>%
      head()
    
    p <- data %>%
      head(30) %>%
      ggplot(aes(x=wt, y=mpg)) +
      geom_point() +
      geom_text( 
        label=rownames(data),
        nudge_x = 0.25, nudge_y = 0.25, 
        check_overlap = T
      )
    ggplotly(p)
  })
  
  output$spg <- renderPlot({
    
    iris %>%
      ggplot(aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
      geom_point(size=6) +
      theme_bw()
  })
  
  output$spg2 <- renderPlot({
    
    iris %>%
      ggplot(aes(x=Sepal.Length, y=Sepal.Width, shape=Species,color=Species)) + 
      geom_point(size=6) +
      theme_bw()
  })
  
  output$bp <- renderPlot({
  
    ggplot(data, aes(x=gear, y=disp)) + 
      geom_bar(stat = "identity", width=0.2) +
      theme_bw()
  })
  
  output$bp1 <- renderPlot({
    
    ggplot(data, aes(x=gear, y=hp)) + 
      geom_bar(stat = "identity") +
      theme_bw()
  })
  
  output$bxp <- renderPlot({
    ggplot(mpg, aes(x=class, y=hwy, fill=class)) + 
      geom_boxplot(alpha=0.3) +
      theme(legend.position="none")
  })
  
  output$ap <- renderPlot({
    ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
      geom_point() +
      scale_x_log10() +
      theme_bw() +
      labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
      transition_time(year) +
      ease_aes('linear')
  })
  
}

shinyApp(ui, server)