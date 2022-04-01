# load libraries
library(tidyverse)
library(stringr)
library(dplyr)

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

#Jocelyn portion of Wrangling:
cp_bdd_survey_data <- select(bdd_survey_data, -c(Status, UserLanguage, DistributionChannel))

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


# goal to replace the spaces with /
  # convert to same category example (She/her) and (her/she) be the same 
  # issue (spaces before and after the "/")
  # question: what to do with (her and they) or (he and they) or (they/them) or (IT) instances 
  # how to group them or categorize them ? 

tables <- table(cp_bdd_survey_data$Q23_modified)
tables # table to show the different output 












