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
# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
#view(subset_bdd_data)
#=======


#>>>>>>> 4b4a23376f977ff2ec8692cc93344c67b6fb3229

# exploring data Nate and Nizan 
summary(cp_bdd_survey_data)
glimpse(cp_bdd_survey_data)

#creating histogram of BDD scores
scores <- cp_bdd_survey_data$BDD_Score
hist(scores,
	main = "Distribution of BDD Scores",
	xlab = "BDD_Scores",
	col = "blue",
	breaks =20)

# exploring questions of interest 

#20
top_ten_ssmedia <- subset_bdd_data %>%
  count(Q20) %>%
  top_n(5) %>%
  ggplot(aes(x = Q20, y = n))+
  geom_col()

#Q22
top_ten_hour_day <- subset_bdd_data %>%
  count(Q14) %>%
  top_n(10) %>%
  ggplot(aes(x= Q14, y = n))+
  geom_col()



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



#To see different apps change the just change the name of the app within
#the code below


#       ⬇change
effect("tiktok", bdd_linear_model) %>%
  data.frame() %>%
#		   ⬇change
  ggplot(aes(x = tiktok,
             y = fit,
             ymin = lower,
             ymax = upper)) +
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




