
# Central Question here is: Was there a decrease in positions that closed between January 2020 and March 2021 
# compared prior year trends? 

# We need to extract the relevant data first.
# We want tenure track faculty positions specifically for early career applicants

# Task 1: Identify all entries that are for tenure track faculty positions for early career applicants. 
ten_track_data <- herc_data %>% 
  filter(Country == "USA") %>% 
  filter(TenureTrack == "Yes") %>% 
  filter(fixed_term == "No") %>% 
  filter(!is.na(YearPosted)) %>% 
  mutate(YearPosted = as.factor(YearPosted)) 

#All tenure track positions
ten_track_summary <- ten_track_data %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())


#All Assistant Professor Tenure Track positions
ecr_summary <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())

#US assistant prof positions by year and region---- (D)
ecr_region_data <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  #select(YearPosted, MonthPosted, US_region, NumPositions) %>% 
  count(YearPosted, US_region, NumPositions) %>% 
  spread(NumPositions, n) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0),
         `5` = 5*`5`,
         `5` = coalesce(`5`, 0),
         n = `2`+`3`+`5`+`<NA>`) %>% 
  select(-`2`, -`3`, -`5`, -`<NA>`) %>% 
  filter(!is.na(US_region)) %>% 
  spread(YearPosted, n)

ecr_region_data[is.na(ecr_region_data)] <- 0

ecr_region_summary <- ecr_region_data %>% 
  mutate(percent_18 = get_percent(`2018`, sum(`2018`)),
         percent_19 = get_percent(`2019`, sum(`2019`)),
         percent_20 = get_percent(`2020`, sum(`2020`)),
         percent_21 = get_percent(`2021`, sum(`2021`))
         #US_region = paste0(US_region, " (n=", n, ")")
  ) %>% 
  rowwise() %>% 
  mutate(Average = round(mean(percent_18,percent_19,percent_21), digits = 2)#,
         #Stdev = round(sd(c(`2018`, `2019`, `2021`)), digits = 2)
  ) %>% 
  gather(-US_region, key = "Year", value = "Percent") %>% 
  filter(Year %not_in% c("2018", "2020", "2019", "2021")) %>% 
  mutate(Year = case_when(
    Year == "percent_18" ~ "2018",
    Year == "percent_19" ~ "2019", 
    Year == "percent_20" ~ "2020",
    Year == "percent_21" ~ "2021",
    TRUE ~ Year))

# ---- (E)
ecr_uni_data <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  select(YearPosted, PUI_RI, NumPositions) %>% 
  count(YearPosted, PUI_RI, NumPositions) %>% 
  spread(NumPositions, n) %>% 
  filter(!is.na(PUI_RI)) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0), 
         `5` = 5*`5`,
         `5` = coalesce(`5`, 0),
         n = `2`+`3`+`5`+`<NA>`) %>% 
  select(-`2`, -`3`, -`5`, -`<NA>`) %>%  
  spread(YearPosted, n) 

ecr_uni_summary <- ecr_uni_data %>% 
  mutate(percent_18 = get_percent(`2018`, sum(`2018`)),
         percent_19 = get_percent(`2019`, sum(`2019`)),
         percent_20 = get_percent(`2020`, sum(`2020`)),
         percent_21 = get_percent(`2021`, sum(`2021`)),
         #US_region = paste0(US_region, " (n=", n, ")")
  ) %>% 
  rowwise() %>% 
  mutate(Average = round(mean(percent_18,percent_19,percent_21), digits = 2)#,
         #Stdev = round(sd(c(`2018`, `2019`, `2021`)), digits = 2)
  ) %>% 
  gather(-PUI_RI, key = "Year", value = "Percent") %>% 
  filter(Year %not_in% c("2018", "2020", "2019", "2021")) %>% 
  mutate(Year = case_when(
    Year == "percent_18" ~ "2018",
    Year == "percent_19" ~ "2019", 
    Year == "percent_20" ~ "2020",
    Year == "percent_21" ~ "2021",
    TRUE ~ Year))
