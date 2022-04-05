#How many onsite interviews were moved to remote?


interview_data <- app_outcomes %>% 
  select(id, on_site_interviews, off_site_interviews, covid_remote) %>% 
  mutate_at(c("on_site_interviews", "off_site_interviews", "covid_remote"), as.numeric)

interview_summary <- interview_data %>% summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))

percent_moved <- interview_summary %>% 
  mutate(per_moved = (covid_remote/on_site_interviews)*100) %>% 
  pull(per_moved) %>% 
  round()

num_moved <- interview_summary %>% pull(covid_remote)

#did this impact offers recieved?


