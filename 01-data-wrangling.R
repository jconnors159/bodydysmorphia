# load libraries
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

# read data in
survey_data <- read_csv("social_media_033022.csv")

survey_data_clean <- survey_data %>%
  slice(-1, -2) %>%
  filter(DistributionChannel == "anonymous" &
           Progress == 100)

# look at data
survey_data_clean %>%
  count(DistributionChannel)

# save clean file to disk (only need to be run once)
write_csv(survey_data_clean, "survey_data_clean.csv")


bdd_survey_data <- read_csv("survey_data_clean.csv")

# Nate's portion of cleaning:
bdd_survey_data <- bdd_survey_data[-c(1,2),]
bdd_survey_data <- filter(bdd_survey_data, Q1 != "I do not accept to participate in this research project")

# Nizan portion of wrangling:
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
#cp_bdd_survey_data %>% count(Q23_final, Q23_modified) %>% view()

#Jocelyn portion of Wrangling: # changed df you selected
cp_bdd_survey_data <- select(cp_bdd_survey_data, -c(Status, UserLanguage, DistributionChannel, Progress, RecordedDate, Q_RecaptchaScore, Finished))

#view(cp_bdd_survey_data)

#Hayli kNN code:

#Preprocessing data w/ one-hot encoding
set.seed(888)
cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(BDD_Categories = case_when(BDD_Score < 20 ~ "low",
                                    BDD_Score >= 20 ~ "high"))
bdd_k <- cp_bdd_survey_data
bdd_dummy <- dummyVars(BDD_Categories ~  tiktok + youtube + instagram + facebook + snapchat + pinterest, data = bdd_k, fullRank = TRUE)
source("KNN_Code.R")
final_plot
bdd_dummy <- dummyVars(BDD_Categories ~  Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11, data = bdd_k, fullRank = TRUE)
source("KNN_Code.R")
final_plot
#<<<<<<< HEAD
# process of data exploration:
#=======

#>>>>>>> 4b4a23376f977ff2ec8692cc93344c67b6fb3229

#<<<<<<< HEAD
#=======


#>>>>>>> 4b4a23376f977ff2ec8692cc93344c67b6fb3229




# exploring data Nate and Nizan 
summary(cp_bdd_survey_data)
glimpse(cp_bdd_survey_data)
# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
#view(subset_bdd_data)

#Exploring data starts here:
#
#creating histogram of BDD scores sep by pronouns
# add labels and axis points 
cp_bdd_survey_data %>%
  ggplot(aes(x= BDD_Score,
         color = Q23_final,
         fill = Q23_final))+
  geom_histogram(binwidth = 2, alpha= 0.5, position = "dodge")+
  scale_color_brewer(palette="Set1")

  
# exploring questions of interest 

#21  class standing by usage of social media platform 
# subset copy of data frame : 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))

new_frame <-subset_bdd_data %>% separate_rows(Q13) %>% 
  group_by(Q21,BDD_Score,Q13) %>% mutate(count =n())

unique(new_frame$count)

new_frame %>%
filter(Q21 != "Other")%>%
ggplot(aes( x = Q21,
          fill = Q13))+
geom_bar() 
  #facet_wrap(~Q13, scales = "free_y") +
  #labs(caption = "note that the scale differs across subplots")
#scale_color_brewer(palette="Set2")# ask adrianna on how to convert to bar 
  

#Q16 topics of usage box plot by age # needs work how show interactively by age group 
Q16_df <- subset_bdd_data%>%
  separate_rows(Q16)%>%group_by(Q16,BDD_Score,Q20)%>%mutate(count =n())%>%
  mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))

Q16_df%>%
  ggplot(aes(x = Q16, y = BDD_Score, color=Q20))+
  geom_boxplot()+coord_flip()

#Exploring data ends here:


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
summary(bdd_linear_model)



bdd_linear_model_gender <- lm(BDD_Score ~ Q22,
                       data = cp_bdd_survey_data)

bdd_linear_model_age <- lm(BDD_Score ~ Q20,
                              data = cp_bdd_survey_data)

bdd_linear_model_time <- lm(BDD_Score ~ Q14,
                           data = cp_bdd_survey_data)

summary(bdd_linear_model_gender)
summary(bdd_linear_model_age)
#time has a statistically significant t-score
summary(bdd_linear_model_time)



effect("Q14", bdd_linear_model_time) %>%
  data.frame() %>%
#		   â¬change
  ggplot(aes(x = Q14,
             y = fit,
             ymin = lower,
             ymax = upper)) +
  xlab("Time Spent on Social Media per day")+
  ylab("Average BDD Score")+
  labs(title="Average BDD score based on hours spent on social media daily")+
  geom_point() +
  geom_errorbar()

#Logistic Regression

#adding a column 

cp_bdd_survey_data <- cp_bdd_survey_data %>%
  mutate(Group = 
           case_when(BDD_Score >= 30 ~ "1",
                     BDD_Score < 30 ~ "0"))


cp_bdd_survey_data <- cp_bdd_survey_data %>%
  rename(BDD_Binary = Group)

#Changing the column from a character to numeric
cp_bdd_survey_data$BDD_Binary <- as.numeric(as.character(cp_bdd_survey_data$BDD_Binary))


log_df <- select(cp_bdd_survey_data, BDD_Score,BDD_Binary)
head(log_df)


#bdd_log_model <- glm(BDD_Binary ~ tiktok + facebook + instagram + pinterest + youtube + snapchat,
#               data = cp_bdd_survey_data)
summary(bdd_log_model)

#Created possibly the ugliest logistic reg graph I have ever seen. Sorry about that, help with making it prettier would be appreciated...
ggplot(cp_bdd_survey_data, aes(x=BDD_Score, y=BDD_Binary)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),
              col="red", lty=2)




demograph_data <- subset_bdd_data[,18:22] %>% rownames_to_column()
newdata <- subset_bdd_data[,2:10]
newdata <- newdata %>% mutate_if(is.character,as.factor)
newdata <- one_hot(as.data.table(newdata))
km <- kmeans(newdata, centers = 2)

#set.seed(123)
#fviz_nbclust(newdata, kmeans, method = "wss")

fviz_cluster(km, newdata, geom = "point")

# compares clusters w/ boxplots
#km$centers
#table(demograph_data$Q22, km$cluster)
#demograph_data$cluster <- km$cluster
#demograph_data %>% count(Q22,cluster)
#demograph_data %>% 
  #ggplot(aes(x = Q22, y = BDD_Score)) + geom_boxplot() + 
  #facet_wrap(~cluster)




