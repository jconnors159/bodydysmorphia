## app.R ##
library(tidyverse)
library(stringr)
library(dplyr)
library(ISLR)
library(caret)
library(ggplot2)
library(e1071)
library(mltools)
library(data.table)
library(cluster)
library(ggplot2)
library(factoextra)
library(RANN)
library(effects)
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

bdd_survey_data <- bdd_survey_data[-c(1,2),]
bdd_survey_data <- filter(bdd_survey_data, Q1 != "I do not accept to participate in this research project")

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

#cp_bdd_survey_data <- select(cp_bdd_survey_data, -c(Status, UserLanguage, DistributionChannel, Progress, RecordedDate, Q_RecaptchaScore, Finished))
# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
#view(subset_bdd_data)

# Cluster Analysis is all put in render plot's dashboard 
#
#


#Jocelyn - Logistic Regression START -----------------------------------

#Tidying data for logistic regression
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(Group = 
           case_when(BDD_Score >= 30 ~ 1,
                     BDD_Score < 30 ~ 0))
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  rename(BDD_Binary = Group)
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(BDD_Binary = ifelse(BDD_Score < 30,
                             0, 1))
#Logistic Regression Model
log_model <- glm(BDD_Binary ~ Q20,
                 data = cp_bdd_survey_data,
                 family = "binomial")

#summary(log_model)
#Visualization of the model to help see fit
# In dashboard

# Jocelyn - Logistic Regression END ------------------------


#multiple linear regression
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(tiktok = ifelse(grepl("TikTok", Q15),
                         "yes", "no"))

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(youtube = ifelse(grepl("YouTube", Q15),
                          "yes", "no"))

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(instagram = ifelse(grepl("Instagram", Q15),
                            "yes", "no"))

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(facebook = ifelse(grepl("Facebook", Q15),
                           "yes", "no"))

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(snapchat = ifelse(grepl("Snapchat", Q15),
                           "yes", "no"))

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(pinterest = ifelse(grepl("Pinterest", Q15),
                            "yes", "no"))

bdd_linear_model <- lm(BDD_Score ~ tiktok + facebook + instagram + pinterest + youtube + snapchat,
                       data = cp_bdd_survey_data)
bdd_linear_model_gender <- lm(BDD_Score ~ Q22,
                              data = cp_bdd_survey_data)
bdd_linear_model_age <- lm(BDD_Score ~ Q20,
                           data = cp_bdd_survey_data)
bdd_linear_model_time <- lm(BDD_Score ~ Q14,
                            data = cp_bdd_survey_data)
#time has a statistically significant t-score
#summary(bdd_linear_model_time)


#Hayli KNN Code (Commit Test)
set.seed(888)
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(BDD_Categories = case_when(BDD_Score < 20 ~ "low",
                                    BDD_Score >= 20 ~ "high"))
print(colnames(cp_bdd_survey_data))
bdd_k <- cp_bdd_survey_data

bdd_dummy <- dummyVars(BDD_Categories ~  tiktok + youtube + instagram + facebook + snapchat + pinterest, data = bdd_k, fullRank = TRUE)
print((bdd_dummy))
source("KNN_Code.R")
kknplot1 <- final_plot

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(BDD_Categories = case_when(BDD_Score < 20 ~ "low",
                                    BDD_Score >= 20 ~ "high"))
bdd_k <- cp_bdd_survey_data
bdd_dummy <- dummyVars(BDD_Categories ~  Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11, data = bdd_k, fullRank = TRUE)
source("KNN_Code.R")
kknplot2 <-final_plot










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
             menuSubItem("Linear Regression", tabName = "linear_regression"),
             menuSubItem("Logistic Regression", tabName = "logistic_regression"),
             menuSubItem("Cluster Analysis", tabName = "cluster_analysis"),
             menuSubItem("kNN Analysis", tabName = "knn_analysis"))
             
             
    )
  )



body <-   dashboardBody(
  
  tabItems(
    # page 1 ----
    tabItem(tabName = "Dashboard", "Dashboard content.",
                            box(h3("BDD Scores across Genders"),plotlyOutput("plot1"),
                                HTML("<p>For the Shiny dashboard we will insert graphs into the statistical analysis portions:</p>
                                      <ul><li> Line of code where the graph is</li>
                                      <li>Summary and results of findings (will be placed underneath the graph) (paragraph) 
                                      Title of graph plot <> X, Y  label names you would like to include 
                                      Legend title (what your filter may be by </li></ul>
")), # string name must match with sever 
            HTML("<br>"),                
            box(h3("Class Standing across Social Media Platforms "), plotlyOutput("plot2")),
                            box(h3("Topics explored on Social Media"), plotlyOutput("plot3"))
              ),
    
    # page 2 ----
    tabItem(tabName = "Widgets", "Widgets content.",
            fluidRow(
              #box(plotOutput("plot1", height = 250))
            )),
    
    
    # page 3 ----
    tabItem(tabName = "linear_regression",
            fluidRow(h3("Average BDD score based on hours spent on social media daily"),
              box(plotOutput("multiplot")),
              h1("Linear Regression content."))),
    # page 4 ----
    tabItem(tabName = "logistic_regression",
            fluidRow(
              fluidPage(
                box(h3(" “Average BDD Score Based on Age Range”"),plotOutput("log_boxplot")),  
                box(h3("“Probability of Age Influencing BDD Score”"), plotOutput("log_sumplot")),
              h1("Logistic Regression content.")))),
    
    # page 5 ----
    tabItem(tabName = "cluster_analysis",
            fluidRow(box(h3("Cluster Analysis of Average BDD Scores"),plotOutput("clusterplot")),
              h1("Cluster Analysis content."))),
    # page 6 ----
    tabItem(tabName = "knn_analysis", 
            fluidRow(box(h3("KNN Analysis with Social Media Platforms"), plotOutput("kkn1")),
            fluidRow(box(h3("KNN Analysis with BDD Questionnaire Questions"), plotOutput("kkn2")),
            h1("kNN Analysis content."))))
    
  )
)



ui <- dashboardPage(header, sidebar, body, skin = "purple")




server <- function(input, output, session) {
  
  # Exploring plot
  output$plot1 <-  renderPlotly({
    
    #Exploring data starts here:
    #creating histogram of BDD scores sep by pronouns
    # add labels and axis points 
    gplot1 <- cp_bdd_survey_data %>%
      rename(Pronouns = Q23_final)%>%
      ggplot(aes(x= BDD_Score,
                 color = Pronouns,
                 fill = Pronouns))+
      geom_histogram(binwidth = 2, alpha= 0.5, position = "dodge")+
      scale_color_brewer(palette="Set1")+
      xlab("BDD Score")+ylab("Amount")
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
      geom_bar()+
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
      geom_boxplot()+coord_flip()+
      ylab("BDD Scores")+xlab("Entertainment")+guides(fill=guide_legend("Age group"))
    gplot3 <-ggplotly(gplot3)
  })
  
  ## KNN plots 
  
  output$kkn1 <- renderPlot({
    kknplot1
  })
  output$kkn2 <- renderPlot({
    kknplot2
  })
  
  
  # logistic plots 
  output$log_sumplot <- renderPlot({
    
    effect("Q20", log_model) %>%
      data.frame() %>%
      ggplot(aes(y = fit,
                 x = Q20)) +
      geom_col() +
      geom_label(aes(label = format(fit, digits = 2)))
  })
  output$log_boxplot <- renderPlot({  
    ggplot(cp_bdd_survey_data, aes(x=BDD_Score, y=Q20)) +
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4) +
      coord_flip() +
      stat_summary(fun=mean, geom="point", shape=23, size=4)
  })
  
  # clusterplot
  output$clusterplot <- renderPlot({
    set.seed(123)
    demograph_data <- subset_bdd_data[,18:22] %>% rownames_to_column()
    newdata <- subset_bdd_data[,2:10]
    newdata <- newdata %>% mutate_if(is.character,as.factor)
    newdata <- one_hot(as.data.table(newdata))
    km <- kmeans(newdata, centers = 2)
    #km
    fviz_cluster(km, newdata, geom = "point")
    })
  
  
  # mulit linear or not plot ?
  output$multiplot <- renderPlot({
    effect("Q14", bdd_linear_model_time) %>%
      data.frame() %>%
      ggplot(aes(x = Q14,
                 y = fit,
                 ymin = lower,
                 ymax = upper)) +
      xlab("Time Spent on Social Media per day")+
      ylab("Average BDD Score")+
      geom_point() +
      geom_errorbar()
  })
  
  
}


shinyApp(ui, server)




