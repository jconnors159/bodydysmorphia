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

view(cp_bdd_survey_data)


# process of data exploration:
# bar charts 
# histogram of questions of interest:
summary(cp_bdd_survey_data)
glimpse(cp_bdd_survey_data)


# data columns of interest 
subset_bdd_data <-cp_bdd_survey_data %>%
  select (Q1:Q23_final, -c(Q23_modified, Q23))
view(subset_bdd_data)


# exploring 
top_ten_ssmedia <- subset_bdd_data %>%
  count(Q20) %>%
  top_n(5)

#Q20 
top_ten_ssmedia %>%
  ggplot(aes(x = Q20, y = n))+
  geom_col()

#Q22
top_ten_hour_day <- subset_bdd_data %>%
  count(Q14) %>%
  top_n(10)

top_ten_hour_day%>%
  ggplot(aes(x= Q14, y = n))+
  geom_col()







