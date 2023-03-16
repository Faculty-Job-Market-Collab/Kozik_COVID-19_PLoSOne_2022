#generate COVID-specific dataset
library(tidyverse)

#arrange data and split into data sets----
clean_data <- read_csv("data/cleaned_data_19-22_2023-03-10.csv") #load survey data

covid_only <- select(clean_data, contains("covid"), id) %>% distinct() #select covid-related questions

other_data <- select(clean_data, id, adjusted_gender, research_category, peer,
                     faculty_offers, offer_responses, first_gen_phd,
                     mental_health_impact, commitment_impact,
                     R1_apps_submitted, PUI_apps_submitted,
                     on_site_interviews, off_site_interviews) %>% distinct()

covid_data <- full_join(covid_only, other_data, by = "id") %>% distinct()

write_csv(covid_data, "data/covid_survey_data_19-22.csv")

#generate data set of faculty offer institutions----
inst_data <- read_csv("data/full_survey_inst_data_19-22.csv")

offers_df <- filter(inst_data, inst_type == offer_inst)
  
write_csv("data/carn_offer_data.csv")