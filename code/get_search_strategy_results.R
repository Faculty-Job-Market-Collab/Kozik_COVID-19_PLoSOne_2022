library(tidyverse)
library(readxl)

strategy_data_raw <- read_excel("data/search_strategy_survey_2021_13.21.xlsx") %>% 
  filter(Progress == 100) %>% 
  select(-c(1:8)) %>% 
  rowid_to_column()

colnames(strategy_data_raw) <- c("id", "current_app", "current_position", "research_category",
                             "desired_institution", "altered_research_remote", "altered_teaching_online",
                             "altered_research_covid", "no_inperson_interv", "covid_restrict_geog",
                             "covid_dep_offer", "no_search_reasons", "change_career_reasons")
  
strategy_data_raw <- strategy_data_raw[-8,]

extent_factors <- c("Strongly disagree", "Disagree", "Somewhat disagree", 
                    "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

mid_pan_strategy_data <- strategy_data_raw %>% 
  mutate(reasons = coalesce(no_search_reasons, change_career_reasons),
         reasons = str_replace_all(reasons, ",(?=.*?\\))", ";"),
         reasons = str_replace(reasons, "(?<=\\));", ",")) %>% 
  separate(reasons, 
           into = c("a", "b", "c", "d", "e", "f"), sep = ",") %>% 
  gather(a:f, key = "y", value = "reasons") %>% 
  gather(altered_research_remote:covid_dep_offer, key = "concern", value = "extent") %>% 
  select(-y, -no_search_reasons, -change_career_reasons) %>% 
  mutate(reasons = if_else(is.na(reasons), "none provided", reasons),
         extent_simple = case_when(
             str_detect(extent, "Neither") ~ "Neutral",
             str_detect(extent, "disagree|Disagree") ~ "Disagree", 
             str_detect(extent, "agree|Agree") ~ "Agree",
             is.na(extent) ~ "No response"),
         research_category = fct_collapse(research_category,
                                          "Mathematics & Engineering Sciences" = c("Mathematical & Physical Sciences",
                                                                                   "Engineering", "Computer & Information Sciences"),
                                          "Social & Behavioral Sciences" = c("Humanities", "Social, Behavior, & Economic Sciences")))
  
respondent_summary <- mid_pan_strategy_data %>% 
  group_by(current_app, current_position, research_category) %>% 
  summarise(n = n())
