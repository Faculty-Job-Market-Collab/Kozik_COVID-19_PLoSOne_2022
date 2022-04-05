library(tidyverse)
library(readxl)

strategy_data_raw <- read_excel("data/search_strategy_survey_2021_13.21.xlsx") %>% 
  filter(Progress == 100) %>% 
  select(-c(1:17)) %>% 
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

yes_summary <- mid_pan_strategy_data %>% 
  filter(current_app == "Yes") %>% 
  group_by(concern, extent) %>% 
  summarise(n = n())

yes_sum_plot <- yes_summary %>% 
  filter(concern != "covid_dep_offer") %>% 
  as_tibble() %>% 
  mutate(extent = factor(extent, extent_factors)) %>% 
  ggplot() +
  geom_col(aes(x = fct_rev(extent), y = n)) +
  facet_wrap(~concern) +
  coord_flip()

#position of respondents
position_num <- mid_pan_strategy_data %>% 
  select(id, current_position) %>% distinct() %>% 
  count(current_position) %>% 
  mutate(percent = get_percent(n, sum(n)))

#geography restricted applications
geog_rest <- mid_pan_strategy_data %>% 
  filter(current_app == "Yes" & concern == "covid_restrict_geog") %>% distinct() %>% 
  count(extent_simple) %>% 
  mutate(percent = get_percent(n, sum(n)))

#avoiding in-person interviews
refuse_inperson <- mid_pan_strategy_data %>% 
  filter(current_app == "Yes" & concern == "no_inperson_interv") %>% distinct() %>% 
  count(extent_simple) %>% 
  mutate(percent = get_percent(n, sum(n)))

#affect of institutional response to offer
offer_summary <- yes_summary %>% 
  filter(concern == "covid_dep_offer") %>% 
  distinct() %>% select(-concern) %>% 
  mutate(percent = get_percent(n, sum(n)))

#reasons for not applying
no_summary <- mid_pan_strategy_data %>% 
  filter(str_detect(current_app, "No") == TRUE) %>% 
  count(current_app, reasons)

chx_career <- no_summary %>% 
  filter(current_app == "No, I decided to change career paths and look outside of academia") %>% 
  select(-current_app) %>% 
  mutate(percent = get_percent(n, sum(n)))

wait <- no_summary %>% 
  filter(current_app == "No, I decided to wait to a later time") %>% 
  select(-current_app) %>% 
  mutate(percent = get_percent(n, sum(n)))
