## app.R ##
library(shiny)
library(shinydashboard)
library(fontawesome)

header <- dashboardHeader(title = "Basic dashboard",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Sales Dept",
                                         message = "Sales are steady this month."
                                       ),
                                       messageItem(
                                         from = "New User",
                                         message = "How do I register?",
                                         icon = icon("question"),
                                       ),
                                       messageItem(
                                         from = "Support",
                                         message = "The new server is ready.",
                                         icon = icon("server"),
                                       )
                          ))


###Jocelyn's Part of the code###

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dashboard", icon = icon("tachometer-alt")),
    menuItem("Widgets", tabName = "Widgets", icon = icon("th")),
    menuItem("Statistical Analysis", tabName = "Statistical Analysis", icon = icon("table")),
    menuSubItem("Multi-Linear Regression", tabName = "Multi-Linear Regression"),
    menuSubItem("Logistic Regression", tabName = "Logistic Regression"),
    menuSubItem("Cluster Analysis", tabName = "Cluster Analysis"),
    menuSubItem("kNN Analysis", tabName = "kNN Analysis"),
    menuSubItem("Histogram/Exploratory Data", tabName = "Histogram/Exploratory Data"),
    
#    selectInput("Statistical Analysis","Select a category", 
#             choices = c("Multi-Linear Regression", "Logistic Regression", "Cluster Analysis", "kNN Analysis", "Histogram/Exploratory Data")),

    menuItem("GitHub", href = "https://github.com/jconnors159/bodydysmorphia", icon = icon("code-branch"))
  )
)


body <-   dashboardBody(
  # Boxes need to be put in a row (or column)
#  fluidRow(
    
#  ),
  
  tabItems(
    # page 1 ----
    tabItem(tabName = "Dashboard", "Dashboard content.",
            ###Copy and paste from fluidRow above###
            box(plotOutput("plot1", height = 250)),
            
            box(
              title = "Controls",
              sliderInput("slider", "Number of observations:", 1, 100, 50)
            )),
            ###           ###         ###           ###           ###
    # page 2 ----
    tabItem(tabName = "Widgets", 
            "Widgets content."),
    # page 3 ----
    tabItem(tabName = "Statistical", 
            "Statistical content."),
    # page 4 ----
    tabItem(tabName = "Multi-Linear Regression", 
            "Multi-Linear Regression content."),
    # page 5 ----
    tabItem(tabName = "Logistic Regression", 
            "Logistic Regression content."),
    # page 6 ----
    tabItem(tabName = "Cluster Analysis", 
            "Cluster Analysis content."),
    # page 7 ----
    tabItem(tabName = "kNN Analysis", 
            "kNN Analysis content."),
    # page 8 ----
    tabItem(tabName = "Histogram/Exploratory Data", 
            "Histogram/Exploratory Data content.")
      )
)


ui <- dashboardPage(header, sidebar, body)




server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  


}


shinyApp(ui, server)





