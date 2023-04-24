#How many onsite interviews were moved to remote?----
interview_data <- clean_data %>% 
  select(id, survey_year, on_site_interviews, off_site_interviews, covid_remote) %>% 
  mutate_at(c("on_site_interviews", "off_site_interviews", "covid_remote"), as.numeric)

interview_summary <- interview_data %>% 
  group_by(survey_year) %>% 
  summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))

percent_moved <- interview_summary %>% 
  mutate(per_moved = round((covid_remote/on_site_interviews)*100), digits = 2) %>% 
  select(survey_year, per_moved)

num_moved <- interview_summary %>% select(survey_year, covid_remote)

#did applicants alter their strategy?----
strategy_data <- clean_data %>% 
  mutate(covid_alter = if_else(covid_alter_research == "Yes, significant changes"|covid_alter_research == "Somewhat", "true", "false")) %>% 
  select(id, survey_year, covid_alter, covid_alter_research) %>% distinct()

strategy_summary <- strategy_data %>% 
  group_by(survey_year, covid_alter_research) %>% summarise(n=n())

num_changed <- strategy_summary %>% 
  mutate(covid_alter_research = if_else(is.na(covid_alter_research), 
                                        "No Response", covid_alter_research)) %>% 
  spread(key = covid_alter_research, value = n) %>% 
  mutate(All_changed = Somewhat+`Yes, significant changes`,
         Total = `Not at all`+ All_changed,
         Percent_changed = get_percent(All_changed, Total))

#how many offers were rescinded due to covid?----

add_list <- c("87_2020-2021", "40_2020-2021", "662_2019-2020", 
              "116_2019-2020") #ids where faculty_offers < rescinded

offers_data <- clean_data %>% #correct for people who reported rescinded offers but no faculty offers
  select(id, survey_year, faculty_offers, covid_offers_rescinded, 
         peer, adjusted_gender, research_category) %>% distinct() %>% 
  mutate_at(c("faculty_offers", "covid_offers_rescinded"), as.numeric) %>% 
  #mutate(faculty_offers = if_else(is.na(faculty_offers), 0, faculty_offers)) %>% 
  mutate(total_offers = if_else(faculty_offers == 0 | id %in% add_list, 
                                faculty_offers + covid_offers_rescinded, faculty_offers),
         total_offers = if_else(is.na(covid_offers_rescinded), 
                                faculty_offers, total_offers))

#calculate important stats = response rate, offers made, rescinded, etc.
response_rate <- offers_data %>% 
  count(survey_year, covid_offers_rescinded) %>% 
  spread(key = covid_offers_rescinded, value = n) %>% 
  mutate(`2` = ifelse(is.na(`2`), "0", `2`),
         `3` = ifelse(is.na(`3`), "0", `3`),
         response = `0`+`1`+as.numeric(`2`)+as.numeric(`3`),
         total = response +`<NA>`,
         response_rate = get_percent(response, total)) %>% 
  select(survey_year, response_rate)

offers_made_df <- offers_data %>% 
  select(id, survey_year, total_offers) %>% 
  distinct() %>% 
  group_by(survey_year) %>% 
  summarise(n_offers = sum(total_offers, na.rm = TRUE))

offers_rescinded_df <- offers_data %>% 
  select(id, survey_year, covid_offers_rescinded) %>% 
  distinct() %>% 
  group_by(survey_year) %>% 
  summarise(n_rescinded = sum(covid_offers_rescinded, na.rm = TRUE))

percent_rescinded_df <- full_join(offers_made_df, offers_rescinded_df, by = "survey_year") %>% 
  mutate(percent_rescinded = get_percent(n_rescinded, n_offers))

#all years pooled
overall_offers_made <- sum(offers_made_df$n_offers)
overall_offers_rescinded <- sum(offers_rescinded_df$n_rescinded)
overall_percent_rescinded <- get_percent(overall_offers_rescinded, overall_offers_made)

#did applicants reject offers due to covid?----
#max_resp <- max(offer_response_data$num_resp, na.rm = TRUE) + 1 #use to calculate max num of responses to expect, add one b/c last response will not have a comma
response_data <- clean_data %>% 
  select(id, survey_year, offer_responses) %>%
  distinct()

response_summary <-  response_data %>% #summary of all responses
  count(survey_year, offer_responses) %>% 
  spread(key = survey_year, value = n) %>% 
  mutate(total = `2019-2020`+`2020-2021`+`2021-2022`)

num_offers_rejected <- response_data %>% #number of all respondents who rejected an offer
  filter(str_detect(offer_responses, "Rejected") == TRUE) %>% 
  select(survey_year, id) %>% distinct() %>% 
  count(survey_year)

percent_covid_rejected <- response_data %>% #percent of offers rejected by respondents due to COVID
  filter(str_detect(offer_responses, "COVID") == TRUE) %>% 
  select(survey_year, id) %>% distinct() %>% 
  count(survey_year) %>% 
  full_join(., num_offers_rejected, by = "survey_year") %>% 
  rename("n_covid_rejected" = n.x, "n_all_rejected" = n.y) %>% 
  mutate(percent_covid_rejected = get_percent(n_covid_rejected, n_all_rejected))

#Gender, race, field, position
res_demo_data <- offers_data %>% 
  filter(total_offers > 0) %>% 
  mutate(covid_offers_rescinded = 
           if_else(is.na(covid_offers_rescinded)|covid_offers_rescinded == 0, 
                   "false", "true"))


# D, E, & F. Comparing applications submitted vs offers received & rescinded----
submitted <- clean_data %>% 
  select(id, survey_year, RI_apps_submitted, PUI_apps_submitted, apps_submitted) %>% distinct() %>% 
  group_by(survey_year) %>% 
  summarise(sum_PUI = sum(as.numeric(PUI_apps_submitted)), 
            sum_RI = sum(as.numeric(RI_apps_submitted)), 
            sum_total = sum(apps_submitted))


#list of respondent ids where not all offer inst were provided
drop_res_id <- c("87_2020-2021", "40_2020-2021", "709_2019-2020", 
                 "701_2019-2020", "662_2019-2020", "625_2019-2020",
                 "24_2019-2020", "786_2019-2020", "816_2019-2020",
                 "782_2019-2020")

#dataset of rescinded offers in 2019-2020 where all offers can be accounted for with institutions
rescinded_df <- left_join(offers_data, offers_df, by = "id") %>% #offers_data for institutions that extended offers, then joined with carnegie data
  filter(survey_year == "2019-2020") %>% 
  filter(id %not_in% drop_res_id) %>% 
  filter(covid_offers_rescinded > 0) %>% 
  #filter(covid_offers_rescinded <= total_offers) %>% 
  select(id, total_offers, covid_offers_rescinded, 
         inst_id, PUI_RI, US_region, world_region) %>% 
  distinct()

percent_rescinded <- percent_rescinded_df[1,4] %>% as.numeric()

#D. Compare the % of R1 vs PUI offers made vs offers rescinded----
offers_inst_type <- rescinded_df %>% 
  filter(!is.na(PUI_RI)) %>% 
  select(-covid_offers_rescinded) %>% 
  group_by(PUI_RI) %>% 
  summarise(n_offers = sum(total_offers))

rescinded_inst_type <- rescinded_df %>% 
  filter(!is.na(PUI_RI)) %>% 
  group_by(PUI_RI) %>% 
  summarise(n_rescinded = sum(covid_offers_rescinded)) #%>% 
  #group_by(survey_year, PUI_RI) %>% summarise(total = sum(n)) %>% as_tibble() %>% 
  #mutate(n_rescinded = ifelse(PUI_RI == "RI", total+4, total)) %>% 
  #select(-total)

PUI_RI_rescinded <- full_join(offers_inst_type, rescinded_inst_type, 
                              by = c("PUI_RI")) %>% 
  mutate(percent_res = get_percent(n_rescinded, n_offers),
         PUI_RI = paste0(PUI_RI, "\n(n=", n_rescinded, "/", n_offers, ")"),
         r = paste0("r=", n_rescinded),
         nr = n_offers - n_rescinded)

#F. Compare US region of institutions applied to and the number of offers rescinded----
offers_US_region <- rescinded_df %>% 
  filter(!is.na(US_region)) %>% 
  select(-covid_offers_rescinded) %>% 
  group_by(US_region) %>% 
  summarise(n_offers = sum(total_offers))

rescinded_US_region <- rescinded_df %>% 
  filter(!is.na(US_region)) %>% 
  group_by(US_region) %>% 
  summarise(n_rescinded = sum(covid_offers_rescinded))

per_US_region_rescinded <- full_join(offers_US_region, rescinded_US_region, 
                                     by = "US_region") %>% 
  mutate(n_rescinded = replace(n_rescinded, is.na(n_rescinded), 0),
         percent_res = get_percent(n_rescinded, n_offers),
         US_region = paste0(US_region, "\n(", n_rescinded, "/", n_offers, ")"),
         r = paste0("r=", n_rescinded),
         nr = n_offers - n_rescinded)