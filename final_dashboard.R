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
  mutate(Q23_final = case_when(grepl("THEY", Q23_modified) ~ "They",
                               grepl("SHE", Q23_modified) ~ "She",
                               TRUE ~ "He"))

#cp_bdd_survey_data <- select(cp_bdd_survey_data, -c(Status, UserLanguage, DistributionChannel, Progress, RecordedDate, Q_RecaptchaScore, Finished))
# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
subset_bdd_data <- subset_bdd_data%>%
  separate_rows(Q16)

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
    tabItem(tabName = "Dashboard",fluidPage(
                                fluidRow( column(11,
                                box(width = '11',
                                h3("BDD Scores Across Genders"), plotlyOutput("plot1"),
                                HTML("<br>"),
                                HTML("<br>"),
                                HTML("<p>“BDD Scores across Genders” is a histogram that looks at the distribution of BBD Scores by 
                                personal pronouns (he, she, or they). The graph shows more of a distribution with a wide range of spread with 
                                one peak value demonstrated on the “she” pronouns with a score of 30.00. A peak from male-identified 
                                pronouns had a score of 21.3. Lastly, the “they” pronouns with a score of 22.6.</p>
")))), # string name must match with sever 
                                HTML("<br>"), 
                                fluidRow(column(11, 
                                box(width = '11', 
                                h3("Class Standing Across Social Media Platforms "), plotlyOutput("plot2"),
                                HTML("<br>"),
                                HTML("<br>"),
                                HTML("<p>“Class Standing across Social Media Platforms” identifies what each class standing utilizes, 
                                     as well as what platform, and opens the question of interest on what social media platforms should 
                                     be used in further analysis. Overall, the graph shows that Instagram is the most used application 
                                     among Freshmen, Sophomores, and Juniors, with Seniors being the only exception of having a higher 
                                     count on YouTube.<p>")))),
                                fluidRow(column(11,
                                box(width = '11',
                                h3("Topics Explored on Social Media"), plotlyOutput("plot3"),
                                HTML("<br>"),
                                HTML("<br>"),
                                HTML("<p>“Topics Explored on Social Media” showcases the BDD scores' distribution across different 
                                     entertainment sources explored on social media. This is indexed by the Age group. There is a 
                                     higher shift of scores in the Technology/Smartphone section in the age group range of 18-20  
                                     versus the 25+ age range showing a higher distribution in the topic of Celebrities.  <p>"), style = "font-size:16px",
                                HTML("<br>"))
              )))),

    
    
    # page 3 ----
    tabItem(tabName = "linear_regression",
              fluidRow(column(11,
              box( width = "11",
                  h3("Average BDD Score Based on Hours Spent on Social Media Daily"), plotOutput("multiplot"),
                  HTML("<br>"),
                  HTML("<br>"),
                  HTML("<p> This graph shows the average BDD score of the participants of the survey based on how much time 
                  they spend on social media daily. There were three given options on the question, less than 3 hours/day, 
                  3-10 hours/day and 10-20 hours/day. The only significant value of the three was 10-20 hours/day with a p-value 
                  of 0.0437 and had an average BDD Score of 10 points higher than both of the other options . We also found that 
                  there was only a .1 difference in average BDD scores of those who spent 3 - 10 hours a day on social media versus 
                  those who spend less than 3 hours a day.  The r-squared value for the model comes out to 0.03 meaning that time 
                  spent on social media explains approximately 3% of the variance in BDD scores. While this is helpful to understand 
                  how time spent on social media can affect BDD scores, since there is a low level of confidence therefore we are unable 
                  to say that time spent on social media daily has an effect on BDD score.
          <p>"))),
              )),
    # page 4 ----
    tabItem(tabName = "logistic_regression",
            fluidPage(
              fluidRow( column(11, 
                box(width='11',h3("Average BDD Score Based on Age Range"),plotOutput("log_boxplot"),
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML("<p>The purpose of the first graph is to show the average BDD Score based on each of the three age 
                        ranges and if someone in the younger adult age (18-20) range would have a higher score than someone in one 
                        of the two other ranges (21-24 or 25+). The only significant p-value among all variables was 18-20, which had 
                        a p-value of 0.01. This means that there is some correlation between this age group and BDD Scores, but more 
                        correlation compared to the age groupings. The p-value of 21-24 was 0.73 and 25+ was 0.69. There is insufficient 
                        evidence to conclude that a relationship exists between the two older age groupings and their effect on the BDD Score. 
                        <p>")))),

                HTML("<br>"),
                HTML("<br>"),
                fluidRow( column(11, 
                  box(width='11',h3("Probability of Age Influencing BDD Score"), plotOutput("log_sumplot"),
                    HTML("<br>"),
                    HTML("<p>This is further proven by looking at the second graph (Probability of Age Influencing BDD Score) 
                        and viewing how each age range has a probability of having an effect on the BDD Score. The effect, or fit, 
                        in this instance is the predicted values given by the logistic regression model. What this means for the plot 
                        above, is that the predicted values (0.34, 0.31, and 0.39) are given based on the outcome, the outcome being 
                        the independent variables (18-20, 21-24, and 25+). The predicted values show the probability of there being an 
                        effect of age on BDD Scores is fairly low for each variable. They all equal about 0.35 on average. 
                        <p>"))),

              ))),
    
    # page 5 ----
    tabItem(tabName = "cluster_analysis",
            fluidRow( column(11,
              box(width='11', h3("Cluster Analysis of Average BDD Scores"),plotOutput("clusterplot"), 
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<p>This model breaks the data into two clusters, or groups, where the first cluster consists of 
                         74 people who took the survey and the second cluster consists of 62 people. This plot shows that the people 
                         in cluster 1 have, on average, a higher BDD score than those in cluster 2. These are individuals who not only 
                         experience appearance-related harassment online and frequently compare themselves to famous people on social 
                         media, but people who also spend, on average, over 3 hours a day on these social networking sites.
                        <p>")))
              )),
    # page 6 ----
    tabItem(tabName = "knn_analysis", 
            fluidPage(
              fluidRow( column(11,
                         box( width = "11",h3("KNN Analysis with Social Media Platforms"), plotOutput("kkn1"),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<p> With all social media platform options (TikTok, YouTube, Instagram, Facebook, 
                             Snapchat, Pinterest) selected as features, KNN analysis shows that the platform an individual 
                             uses doesn’t have a significant impact on BDD score. While the algorithm correctly predicted 11 
                             high scores as high, 8 low scores were incorrectly classified as high. 5 high scores were incorrectly 
                             predicted to be low. Only 3 low scores were predicted correctly by the KNN algorithm. This shows that 
                             the algorithm overdiagnosed many as having a high BDD score when they in fact did not. <p>")))),
                     HTML("<br>"),
            fluidRow( column(11,
                      box(width = "11", h3("KNN Analysis with BDD Questionnaire Questions"), plotOutput("kkn2"),
                             HTML("<br>"),
                             HTML("<br>"),
                             HTML("<p> With all of the BDD-score-determining questions selected as features, KNN much 
                                 more accurately predicts the scores of participants. This, of course, is to be expected. 
                                 15 actual high scores are correctly predicted to be high while only one was incorrectly 
                                 predicted to be low. Meanwhile, 8 low scores were correctly predicted as low while 3 were 
                                 incorrectly predicted as high. While there is still a trend of overdiagnosing low scores as 
                                 high, we see a more accurate analysis made by the KNN algorithm when taking into account BDD 
                                 Test questions.</p>")
                          )))))
    
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
    
    positions <- c("Freshman", "Sophomore", "Junior","Senior")
    
    gplot2 <- new_frame %>%
      filter(Q21 != "Other")%>%
      ggplot(aes( x = Q21,
                  fill = Q13))+
      geom_bar()+
      xlab("Class Standing")+ylab("Amount")+guides(fill=guide_legend("Social Media"))+ 
      scale_x_discrete(limits = positions)
    gplot2 <-ggplotly(gplot2)
  })
  
  output$plot3 <-renderPlotly({
    
    #Q16 topics of usage box plot by age # needs work how show interactively by age group 
    
    Q16_df <- subset_bdd_data%>%
      mutate(Q16 = str_to_title(Q16))%>%
      filter(Q16 != "And" & Q16 !="Topics" & Q16 != "Related" & Q16 != "Video", Q16 != "Body")%>%
      group_by(Q16,BDD_Score,Q20)%>%mutate(count =n())%>%
      mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
    
    
    gplot3 <- Q16_df%>%
      ggplot(aes(x = Q16, y = BDD_Score, fill= Q20))+
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
      geom_errorbar()+
      scale_x_discrete(limits=c("<3 hrs/day", "3 - 10 hrs/day","10 - 20 hrs/day"))

  })
  
  
}


shinyApp(ui, server)




