## app.R ##
library(tidyverse)
library(stringr)
library(dplyr)
library(ISLR)
library(caret)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(fontawesome)
library(plotly)

# read in df
survey_data <- read_csv("social_media_033022.csv")

# starting to clean data 
survey_data_clean <- survey_data %>%
  slice(-1, -2) %>%
  filter(DistributionChannel == "anonymous" &
           Progress == 100)

#csv of a clean version 
#write_csv(survey_data_clean, "survey_data_clean.csv")
bdd_survey_data <- read_csv("survey_data_clean.csv")

# Nate's portion of cleaning:
bdd_survey_data <- bdd_survey_data[-c(1,2),]
bdd_survey_data <- filter(bdd_survey_data, Q1 != "I do not accept to participate in this research project")

#Nizan Portion of Wrangling 
cp_bdd_survey_data <- bdd_survey_data
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  filter(Q21 != "Graduate program") %>%  # took out graud program
  mutate(Q23_modified = str_replace_all(Q23,".+(\\s).+", ""))%>% # changed format in Q23
  mutate(Q23_modified = str_replace_all(Q23, ",", "/"))%>%
  mutate(Q23_modified = str_replace_all(Q23, "/(\\s)\1", "/"))%>%  #issue 1
  mutate(Q23_modified = toupper(Q23_modified)) %>%
  mutate(Q23_final = case_when(grepl("THEY", Q23_modified) ~ "they",
                               grepl("SHE", Q23_modified) ~ "she",
                               TRUE ~ "he"))

#Jocelyn portion of Wrangling: # changed df you selected
#cp_bdd_survey_data <- select(cp_bdd_survey_data, -c(Status, UserLanguage, DistributionChannel, Progress, RecordedDate, Q_RecaptchaScore, Finished))

# exploring data Nate and Nizan 
summary(cp_bdd_survey_data)
glimpse(cp_bdd_survey_data)

# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
#view(subset_bdd_data)




#
# Start of the webpage 
#
header <- dashboardHeader(title = "Cross Study of Body Dismorphia and Social Media"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dashboard", icon = icon("tachometer-alt")),
    menuItem("Widgets", tabName = "Widgets", icon = icon("th")),
    menuItem("GitHub", href = "https://github.com/jconnors159/bodydysmorphia", icon = icon("code-branch")),
    
    
    menuItem("Statistical Analysis", tabName = "statistical_analysis", 
             menuSubItem("Multi-Linear Regression", tabName = "multilinear_regression"),
             menuSubItem("Logistic Regression", tabName = "logistic_regression"),
             menuSubItem("Cluster Analysis", tabName = "cluster_analysis"),
             menuSubItem("kNN Analysis", tabName = "knn_analysis"),
             menuSubItem("Histogram/Exploratory Data", tabName = "histogram")
             
             
    )
  )
)


body <-   dashboardBody(
  
  tabItems(
    # page 1 ----
    tabItem(tabName = "Dashboard", "Dashboard content.",
            fluidPage(
              box(plotlyOutput("plot1")), # string name must match with sever 
              box(plotlyOutput("plot2")),
              box(plotlyOutput("plot3")))),
    
    # page 2 ----
    tabItem(tabName = "Widgets", "Widgets content.",
            fluidRow(
              #box(plotOutput("plot1", height = 250))
            )),
    
    
    # page 3 ----
    tabItem(tabName = "multilinear_regression",
            fluidRow(
              #box(plotOutput("plot1", height = 250)),
              h1("Multi-Linear Regression content."))),
    # page 4 ----
    tabItem(tabName = "logistic_regression",
            fluidRow(
              h1("Logistic Regression content."))),
    # page 5 ----
    tabItem(tabName = "cluster_analysis",
            fluidRow(
              h1("Cluster Analysis content."))),
    # page 6 ----
    tabItem(tabName = "knn_analysis", 
            fluidRow(
              h1("kNN Analysis content."))),
    # page 7 ----
    tabItem(tabName = "histogram", 
            fluidRow(
              h1("Histogram/Exploratory Data content.")))
  )
)



ui <- dashboardPage(header, sidebar, body)




server <- function(input, output, session) {
  
  
  output$plot1 <-  renderPlotly({
    
    #Exploring data starts here:
    #creating histogram of BDD scores sep by pronouns
    # add labels and axis points 
    gplot1 <- cp_bdd_survey_data %>%
      ggplot(aes(x= BDD_Score,
                 color = Q23_final,
                 fill = Q23_final))+
      geom_histogram(binwidth = 2, alpha= 0.5, position = "dodge")+
      scale_color_brewer(palette="Set1")+ ggtitle("BDD Scores across Genders\n ")+
      xlab("BDD Score")+ylab("Amount")+guides(fill=guide_legend("Gender Pronouns"))
    gplot1 <- ggplotly(gplot1)
  })
  
  output$plot2 <- renderPlotly({
    
    #21  class standing by usage of social media platform 
    # subset copy of data frame : 
    subset_bdd_data <-cp_bdd_survey_data %>%
      select (Q1:Q23_final, -c(Q23_modified, Q23))
    
    new_frame <-subset_bdd_data %>% separate_rows(Q13) %>% 
      group_by(Q21,BDD_Score,Q13) %>% mutate(count =n())
    
    gplot2 <- new_frame %>%
      filter(Q21 != "Other")%>%
      ggplot(aes( x = Q21,
                  fill = Q13))+
      geom_bar()+ ggtitle("Class Standing across Social Media Platforms ")+
      xlab("Class Standing")+ylab("Amount")+guides(fill=guide_legend("Social Media")) #+
    #facet_wrap(~Q13, scales = "free_y") +
    #labs(caption = "note that the scale differs across subplots")
    #scale_color_brewer(palette="Set2")# ask adrianna on how to convert to bar
    gplot2 <-ggplotly(gplot2)
  })
  
  output$plot3 <-renderPlotly({
    
    #Q16 topics of usage box plot by age # needs work how show interactively by age group 
    Q16_df <- subset_bdd_data%>%
      mutate(Q16_final = str_replace_all(Q16, ".(topics).",""))%>%
      mutate(Q16_final = str_replace_all(Q16,".(and).", ""))%>%
      separate_rows(Q16_final)%>%group_by(Q16_final,BDD_Score,Q20)%>%mutate(count =n())%>%
      mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
    
    gplot3 <- Q16_df%>%
      ggplot(aes(x = Q16_final, y = BDD_Score, fill= Q20))+
      geom_boxplot()+coord_flip()+ggtitle("Topics explored on Social Media\n")+
      ylab("BDD Scores")+xlab("Entertainment")+guides(fill=guide_legend("Age group"))
    gplot3 <-ggplotly(gplot3)
  })
  
  
}


shinyApp(ui, server)




