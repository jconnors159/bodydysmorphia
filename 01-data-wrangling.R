# load libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(ISLR)
library(caret)
library(ggplot2)
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
cp_bdd_survey_data <- select(cp_bdd_survey_data, -c(Status, UserLanguage, DistributionChannel))

#view(cp_bdd_survey_data)

#Hayli kNN code:

#Preprocessing data w/ one-hot encoding
bdd_k <- bdd_survey_data
bdd_dummy <- dummyVars(BDD_Score ~ ., data = bdd_k, fullRank = TRUE)

#<<<<<<< HEAD
# process of data exploration:
#=======
bdd_k <- predict(bdd_dummy, newdata = bdd_k)
bdd_k <- data.frame(bdd_k)
#>>>>>>> 4b4a23376f977ff2ec8692cc93344c67b6fb3229

#Re-adding target variable (BDD_Score) to dataset bc dummyVars dropped it
score_vals <- cp_bdd_survey_data %>%
  select(BDD_Score)
bdd_k <- cbind(bdd_k, score_vals)

#<<<<<<< HEAD
# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
#view(subset_bdd_data)
#=======
#Training data
bdd_split <- createDataPartition(bdd_k$BDD_Score, p = 0.8, list = FALSE)
features_train <- bdd_k[bdd_split, !(names(bdd_k) %in% c('BDD_Score'))]
features_test <- bdd_k[-bdd_split, !(names(bdd_k) %in% c('BDD_Score'))]
target_train <- bdd_k[bdd_split, "BDD_Score"]
target_test <- bdd_k[-bdd_split, "BDD_Score"]
preprocess_object <- preProcess(features_train, 
                                method = c('center', 'scale', 'knnImpute'))
#>>>>>>> 4b4a23376f977ff2ec8692cc93344c67b6fb3229

features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)

#<<<<<<< HEAD
#=======
#Fitting kNN model 
knn_fit <- knn3(features_train, target_train, k = 5)
knn_pred <- predict(knn_fit, features_test, type = 'class' )
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

