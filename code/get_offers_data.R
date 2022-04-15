#how many offers were rescinded due to covid?

add_list <- c("1", "107", "257", "646", "680", "708", "74", "78", "93", "148", "41", "624", "197", "207", "313", 
              "330", "395", "746", "771", "775", "661", "773")

offers_data <- clean_data %>% 
  filter(!is.na(on_site_interviews) & on_site_interviews != 0) %>% 
  mutate_at(c("faculty_offers", "covid_offers_rescinded"), as.numeric) %>% 
  #mutate(faculty_offers = if_else(is.na(faculty_offers), 0, faculty_offers)) %>% 
  mutate(total_offers = if_else(faculty_offers == 0 | id %in% add_list, 
                                faculty_offers + covid_offers_rescinded, faculty_offers),
         total_offers = if_else(is.na(covid_offers_rescinded), faculty_offers, total_offers))

#calulate important stats = response rate, offers made, rescinded, etc.----
response_rate <- offers_data %>% 
  count(covid_offers_rescinded) %>% 
  mutate(rate = get_percent(n, sum(n))) %>% 
  filter(!is.na(covid_offers_rescinded)) %>% 
  summarise(rate = sum(rate)) %>% pull(rate) #75%

offers_made <- offers_data %>% pull(total_offers) %>% sum(., na.rm = TRUE)

offers_rescinded <- offers_data %>% pull(covid_offers_rescinded) %>% sum(., na.rm = TRUE)

percent_rescinded <- (offers_rescinded/offers_made)*100 #need to correct for differing interpretations, some did not include rescinded offers with the offers made

#did applicants reject offers due to covid?

offer_response_data <- offers_data %>% select(id, offer_responses) %>% 
  mutate(num_resp = str_count(offer_responses, ",")) %>% 
  separate(., offer_responses, sep = ",", 
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")) %>% 
  gather(resp1:resp6, key = num, value = response) %>% 
  filter(!is.na(response)) %>% 
  select(-num_resp, -num)

#max_resp <- max(offer_response_data$num_resp, na.rm = TRUE) + 1 #use to calculate max num of responses to expect, add one b/c last response will not have a comma

response_summary <- offer_response_data %>% 
  group_by(response) %>% summarise(n = n())

#12 offers rejected b/c of covid

offers_made <- offers_data %>% pull(total_offers) %>% sum(., na.rm = TRUE)

offers_rescinded <- offers_data %>% pull(covid_offers_rescinded) %>% sum(., na.rm = TRUE)

percent_rescinded <- (offers_rescinded/offers_made)*100 #need to correct for differing interpretations, some did not include rescinded offers with the offers made

#Gender, race, field, position
res_demo_data <- offers_data %>% 
  filter(total_offers > 0) %>% 
  mutate(covid_offers_rescinded = 
           if_else(is.na(covid_offers_rescinded)|covid_offers_rescinded == 0, "false", "true"))


race_data <- res_demo_data %>% 
  select(id, race_ethnicity, covid_offers_rescinded) %>% 
  mutate(race_ethnicity = str_remove(race_ethnicity, "\\(.+\\)")
         ) %>% 
  separate(race_ethnicity, sep = ",", into = c("a", "b")) %>% 
  #ather(a:b, key = "test", value = "race_ethnicity") %>% 
  select(-b) %>% 
  rename(race_ethnicity = "a") %>% 
  mutate(race_ethnicity = fct_collapse(race_ethnicity,
                                       "American/Islander Indigenous" = c("Oceania ", "Not Listed", "North American Indigenous ",
                                                                          "Caribbean Islander ", 
                                                                          "North American Hispanic/Latinx", "South/Central American"))) %>% 
  filter(!is.na(race_ethnicity))

#visa_rescinded <- race_data %>% 
#  count(spons_req, covid_offers_rescinded) %>% 
#    #as_tibble() %>% 
#    spread(key = covid_offers_rescinded, value = n) %>% 
#    mutate(total = true + false,
#           total = if_else(is.na(total), "0", as.character(total)),
#           percent_res = get_percent(true, total)) %>% 
#    select(spons_req, total, percent_res)

# D, E, & F. Comparing applications submitted vs offers received & rescinded----
submitted <- clean_data %>% 
  select(id, R1_apps_submitted, PUI_apps_submitted) %>% 
  mutate(R1_apps_submitted = if_else(is.na(R1_apps_submitted), "0", as.character(R1_apps_submitted)),
         PUI_apps_submitted = if_else(is.na(PUI_apps_submitted), "0", as.character(PUI_apps_submitted)),
         total_apps_submitted = as.numeric(R1_apps_submitted) + as.numeric(PUI_apps_submitted)) %>% 
  summarise(sum_PUI = sum(as.numeric(PUI_apps_submitted)), sum_RI = sum(as.numeric(R1_apps_submitted)), sum_total = sum(total_apps_submitted))

rescinded_df <- offers_df %>% #offers_data for insitutions that extended offers, then joined with carnegie data and filtered for (total_offers > 0)
  filter(covid_offers_rescinded > 0) %>% 
  filter(covid_offers_rescinded <= total_offers) %>% 
  select(id, total_offers, covid_offers_rescinded, PUI_RI, US_region, world_region) %>% 
  distinct()

#D. Compare the % of R1 vs PUI offers made vs offers rescinded----

offers_inst_type <- offers_df %>% 
  filter(!is.na(PUI_RI)) %>% 
  select(-covid_offers_rescinded) %>% 
  group_by(PUI_RI) %>% 
  summarise(n_offers = n())

rescinded_inst_type <- rescinded_df %>% 
  filter(!is.na(PUI_RI)) %>% 
  group_by(PUI_RI, covid_offers_rescinded) %>% summarise(n = n()) %>% 
  group_by(PUI_RI) %>% summarise(total = sum(n)) %>% as.tibble() %>% 
  mutate(n_rescinded = ifelse(PUI_RI == "RI", total+4, total)) %>% 
  select(-total)

PUI_RI_rescinded <- full_join(offers_inst_type, rescinded_inst_type, by = "PUI_RI") %>% 
  mutate(percent_res = get_percent(n_rescinded, n_offers),
         PUI_RI = paste0(PUI_RI, "\n(n=", n_rescinded, "/", n_offers, ")"),
         r = paste0("r=", n_rescinded),
         nr = n_offers - n_rescinded)

#F. Compare US region of institutions applied to and the number of offers rescinded----

offers_US_region <- offers_df %>% 
  filter(!is.na(US_region)) %>% 
  select(-covid_offers_rescinded) %>% 
  group_by(US_region) %>% 
  summarise(n_offers = n())

rescinded_US_region <- rescinded_df %>% 
  filter(!is.na(US_region)) %>% 
  group_by(US_region, covid_offers_rescinded) %>% summarise(n = n()) %>% 
  group_by(US_region) %>% summarise(n_rescinded = sum(n)) %>% as.tibble() 

per_US_region_rescinded <- full_join(offers_US_region, rescinded_US_region, by = "US_region") %>% 
  mutate(n_rescinded = replace(n_rescinded, is.na(n_rescinded), 0),
         percent_res = get_percent(n_rescinded, n_offers),
         US_region = paste0(US_region, "\n(", n_rescinded, "/", n_offers, ")"),
         r = paste0("r=", n_rescinded),
         nr = n_offers - n_rescinded)