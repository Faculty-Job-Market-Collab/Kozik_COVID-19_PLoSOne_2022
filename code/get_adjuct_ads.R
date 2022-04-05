#what happens to adjunct positions?

non_track_data <- herc_data %>% 
  filter(TenureTrack == "No") %>% 
  filter(str_detect(Title, "Lectur|Adjunct|Prof|Facul|Instructor") == TRUE) %>% 
  filter(!is.na(YearPosted)) %>% 
  mutate(YearPosted = as.factor(YearPosted)) 

#All adjunct positions
adjunct_summary <- non_track_data %>% 
  filter(Adjunct == "Yes") %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())

#All non-tenure track faculty positions
fixed_summary <- non_track_data %>% 
  filter(str_detect(Title, "Assistant|Prof") == TRUE) %>% 
  filter(NonTrack == "Yes") %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())


