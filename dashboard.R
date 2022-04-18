## app.R ##
library(shiny)
library(shinydashboard)
library(fontawesome)

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
            fluidRow(
              box(plotOutput("plot1", height = 250)), # string name must match with sever 
              box(plotOutput("plot2", height = 250)),
              box(plotOutput("plot3", height = 250))))
    ,
            ###           ###         ###           ###           ###
    # page 2 ----
    tabItem(tabName = "Widgets", "Widgets content."),
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
  
  
  output$plot1 <- renderPlot({
    
    #Exploring data starts here:
    #creating histogram of BDD scores sep by pronouns
    # add labels and axis points 
    cp_bdd_survey_data %>%
      ggplot(aes(x= BDD_Score,
                 color = Q23_final,
                 fill = Q23_final))+
      geom_histogram(binwidth = 2, alpha= 0.5, position = "dodge")+
      scale_color_brewer(palette="Set1")
    
  })
  output$plot2 <- renderPlot({
    
    #21  class standing by usage of social media platform 
    # subset copy of data frame : 
    subset_bdd_data <-cp_bdd_survey_data %>%
      select (Q1:Q23_final, -c(Q23_modified, Q23))
    
    new_frame <-subset_bdd_data %>% separate_rows(Q13) %>% 
      group_by(Q21,BDD_Score,Q13) %>% mutate(count =n())
    
    new_frame %>%
      filter(Q21 != "Other")%>%
      ggplot(aes( x = Q21,
                  y = count,
                  color = Q13,
                  fill = Q13))+
      geom_col() #+
    #facet_wrap(~Q13, scales = "free_y") +
    #labs(caption = "note that the scale differs across subplots")
    #scale_color_brewer(palette="Set2")# ask adrianna on how to convert to bar
    
  })
  
  output$plot3 <- renderPlot({
    
    #Q16 topics of usage box plot by age # needs work how show interactively by age group 
    Q16_df <- subset_bdd_data%>%
      mutate(Q16_final = str_replace_all(Q16, ".(topics).",""))%>%
      mutate(Q16_final = str_replace_all(Q16,".(and).", ""))%>%
      separate_rows(Q16_final)%>%group_by(Q16_final,BDD_Score,Q20)%>%mutate(count =n())%>%
      mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
    
    Q16_df%>%
      ggplot(aes(x = Q16_final, y = BDD_Score, fill= Q20))+
      geom_boxplot()+coord_flip()
  })


}


shinyApp(ui, server)





