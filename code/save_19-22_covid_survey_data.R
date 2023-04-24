#generate COVID-specific dataset
library(tidyverse)

#arrange data and split into data sets----
clean_data <- read_csv("data/cleaned_data_19-22_2023-03-10.csv") #load survey data

covid_only <- select(clean_data, contains("covid"), id) %>% distinct() #select covid-related questions

other_data <- select(clean_data, id, survey_year, adjusted_gender, 
                     research_category, peer, rejections_received,
                     faculty_offers, offer_responses, first_gen_phd,
                     mental_health_impact, commitment_impact,
                     RI_apps_submitted, PUI_apps_submitted, apps_submitted,
                     on_site_interviews, off_site_interviews) %>% distinct()

covid_data <- full_join(covid_only, other_data, by = "id") %>% distinct()

write_csv(covid_data, "data/covid_survey_data_19-22.csv")

#generate data set of faculty offer institutions----
#dataset for each institution listed w/ pui, ri status, region, etc
inst_data <- read_csv("data/final_survey_inst_data_2019-2022_2023-04-11.csv") 

inst_data <- inst_data %>% 
  select(id, survey_year, inst_type, inst_id, US_region, PUI_RI, 
         world_region, Other_inst_type) 

offers_df <- filter(inst_data, inst_type == "offer_institutions")
  
write_csv(offers_df, "data/carn_offer_data.csv")