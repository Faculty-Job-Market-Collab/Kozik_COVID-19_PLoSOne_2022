#did applicants alther their strategy?

strategy_data <- clean_data %>% 
  mutate(covid_alter = if_else(covid_alter_research == "Yes, significant changes"|covid_alter_research == "Somewhat", "true", "false"))

strategy_summary <- strategy_data %>% 
  group_by(covid_alter_research) %>% summarise(n=n())

num_changed <- strategy_summary[3,2]+strategy_summary[2,2]

percent_changed <- get_percent(num_changed,sum(strategy_summary$n))


#join w/ demo data
#strategy_demo_data <- left_join(strategy_data, demographics, by = "id")

#changed_demo <- strategy_data %>% 
#  filter(covid_alter_research == "Yes, significant changes"|covid_alter_research == "Somewhat")

#join w/ preparations
#strategy_prep_data <- left_join(strategy_data, clean_data, by = "id")

#changed_prep <- strategy_prep_data %>% 
#  filter(covid_alter_research == "Yes, significant changes"|covid_alter_research == "Somewhat")

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
#first gen
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