#install.packages("shinydashboard") # group please install this 
#install.packages("shiny") # to load the website ty
library(shiny)
library(shinydashboard)
#survey_data <- read_csv("social_media_033022.csv")

dash_web_page <- dashboardPage(   
  dashboardHeader(title = "Cross Study of Body Dismorphia and Social Media"),
  dashboardSidebar(),
  # fluidRow for row or column of data of the amount of graphs needed
  dashboardBody(),
  skin = "white"
)

server <- function(input, output) { }

shinyApp(dash_web_page, server)
