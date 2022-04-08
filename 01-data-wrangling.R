# load libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(ISLR)
library(caret)
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
cp_bdd_survey_data %>% count(Q23_final, Q23_modified) %>% view()

#Jocelyn portion of Wrangling:
cp_bdd_survey_data <- select(bdd_survey_data, -c(Status, UserLanguage, DistributionChannel))


#Hayli kNN code:

#Preprocessing data w/ one-hot encoding
bdd_k <- bdd_survey_data
bdd_dummy <- dummyVars(BDD_Score ~ ., data = bdd_k, fullRank = TRUE)

bdd_k <- predict(bdd_dummy, newdata = bdd_k)
bdd_k <- data.frame(bdd_k)

#Re-adding target variable (BDD_Score) to dataset bc dummyVars dropped it
score_vals <- cp_bdd_survey_data %>%
  select(BDD_Score)
bdd_k <- cbind(bdd_k, score_vals)

#Training data
bdd_split <- createDataPartition(bdd_k$BDD_Score, p = 0.8, list = FALSE)
features_train <- bdd_k[bdd_split, !(names(bdd_k) %in% c('BDD_Score'))]
features_test <- bdd_k[-bdd_split, !(names(bdd_k) %in% c('BDD_Score'))]
target_train <- bdd_k[bdd_split, "BDD_Score"]
target_test <- bdd_k[-bdd_split, "BDD_Score"]
preprocess_object <- preProcess(features_train, 
                                method = c('center', 'scale', 'knnImpute'))

features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)

#Fitting kNN model 
knn_fit <- knn3(features_train, target_train, k = 5)
knn_pred <- predict(knn_fit, features_test, type = 'class' )








