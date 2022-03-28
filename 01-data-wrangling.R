# load libraries
library(tidyverse)

# read data in
survey_data <- read_csv("raw-data/Social Media Interaction Survey_March 18, 2022_15.19.csv")

survey_data_clean <- survey_data %>%
  slice(-1, -2) %>%
  filter(DistributionChannel == "anonymous" &
           Progress == 100)

# look at data
survey_data_clean %>%
  count(DistributionChannel)

# save clean file to disk
write_csv(survey_data_clean, "processed-data/survey_data_clean.csv")
