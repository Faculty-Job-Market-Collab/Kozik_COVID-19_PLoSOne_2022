#did applicants alther their strategy?

strategy_data <- clean_data %>% 
  select(id, survey_year, covid_alter_research) %>% distinct() %>% 
  mutate(covid_alter = if_else(covid_alter_research == "Yes, significant changes"|covid_alter_research == "Somewhat", "true", "false"))

strategy_summary <- strategy_data %>% 
  count(survey_year, covid_alter_research) %>% 
  spread(key = covid_alter_research, value = n) %>% 
  mutate(num_changed = Somewhat + `Yes, significant changes`,
         total_resp = `Not at all`+ num_changed,
         percent_changed = get_percent(num_changed, total_resp),
         percent_response = get_percent(total_resp, (total_resp+`<NA>`))) %>% 
  select(-c(2:5))

overall_num_changed <- sum(strategy_summary$num_changed)
overall_total_resp <- sum(strategy_summary$total_resp)
overall_percent_changed <- get_percent(overall_num_changed, 
                                       overall_total_resp)

#join w/ demo data
strategy_demo_data <- clean_data %>% 
  select(id, survey_year, peer, adjusted_gender, research_category,
         first_gen_phd) %>% 
  left_join(., strategy_data, by = c("id", "survey_year")) %>% distinct()

changed_demo_df <- strategy_demo_data %>% 
  filter(!is.na(covid_alter)) %>% distinct()

changed_demo_peer <- changed_demo_df %>% 
  count(survey_year, covid_alter, peer) %>% 
  spread(key = covid_alter, value = n) %>% 
  mutate(Total = true + false,
         `% Adjusted` = get_percent(true, Total),
         `% No Adjustment` = get_percent(false, Total)) %>% 
  rename("No adjustment" = false, "Adjusted" = true)
  

changed_demo_gender <- changed_demo_df %>% 
  count(survey_year, covid_alter, adjusted_gender) %>% 
  spread(key = covid_alter, value = n) %>% 
  mutate(Total = true + false,
         `% Adjusted` = get_percent(true, Total),
         `% No Adjustment` = get_percent(false, Total)) %>% 
  rename("No adjustment" = false, "Adjusted" = true)

changed_demo_research <- changed_demo_df %>% 
  count(survey_year, covid_alter, research_category) %>% 
  spread(key = covid_alter, value = n) %>% 
  mutate(Total = true + false,
         `% Adjusted` = get_percent(true, Total),
         `% No Adjustment` = get_percent(false, Total)) %>% 
  rename("No adjustment" = false, "Adjusted" = true)

changed_demo_first_gen <- changed_demo_df %>% 
  count(survey_year, covid_alter, first_gen_phd) %>% 
  spread(key = covid_alter, value = n) %>% 
  mutate(Total = true + false,
         `% Adjusted` = get_percent(true, Total),
         `% No Adjustment` = get_percent(false, Total)) %>% 
  rename("No adjustment" = false, "Adjusted" = true)


#demo plots----
#position
#get_plot_summary(strategy_demo_data, "position", "covid_alter") %>% 
#  ggplot()+
#  geom_col(aes(x = position, y=percent_res))+
#  coord_flip()+
#  labs(y = "Percent Changed Strategy")
#
##field(s)
#get_plot_summary(strategy_demo_data, "research_category", "covid_alter") %>% 
#  ggplot()+
#  geom_col(aes(x = research_category, y=percent_res))+
#  coord_flip()+
#  labs(y = "Percent Changed Strategy")
#
##first gen
#get_plot_summary(strategy_demo_data, "first_gen_undergrad", "covid_alter") %>% 
#  ggplot()+
#  geom_col(aes(x = first_gen_undergrad, y=percent_res))+
#  coord_flip()+
#  labs(y = "Percent Changed Strategy")
#
#get_plot_summary(strategy_demo_data, "first_gen_phd", "covid_alter") %>% 
#  ggplot()+
#  geom_col(aes(x = first_gen_phd, y=percent_res))+
#  coord_flip()+
#  labs(y = "Percent Changed Strategy")
#
##prep plots ----
##app feedback
#get_plot_summary(strategy_prep_data, "app_feedback", "covid_alter") %>% 
#  ggplot()+
#  geom_col(aes(x = app_feedback, y=percent_res))+
#  coord_flip()+
#  labs(y = "Percent Changed Strategy")
#
##interview feedback
#get_plot_summary(strategy_prep_data, "interview_feedback", "covid_alter") %>% 
#  ggplot()+
#  geom_col(aes(x = interview_feedback, y=percent_res))+
#  coord_flip()+
#  labs(y = "Percent Changed Strategy")#