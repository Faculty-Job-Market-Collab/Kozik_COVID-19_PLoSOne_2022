stats_demo_data <- res_demo_data %>% 
  select(id, covid_offers_rescinded, gender, race_ethnicity, research_category)

#Figure 1 -----

#Fig A
gen_chi <- fisher.test(table(stats_demo_data$gender, 
                             stats_demo_data$covid_offers_rescinded))

#Fig B
cat_chi <- fisher.test(table(stats_demo_data$research_category,
                             stats_demo_data$covid_offers_rescinded))

#Fig C
re_chi <- fisher.test(table(race_data$race_ethnicity, 
                            race_data$covid_offers_rescinded))

#Fig D
pui_count <- PUI_RI_rescinded %>% select(PUI_RI, n_rescinded, nr) %>% 
  remove_rownames %>% column_to_rownames(var="PUI_RI")

pui_chi <- fisher.test(pui_count)

#Fig F
us_reg_count <- per_US_region_rescinded %>% select(US_region, n_rescinded, nr) %>% 
  remove_rownames %>% column_to_rownames(var="US_region") 

us_reg_chi <- fisher.test(us_reg_count)

#Figure 2 ----

stats_ten_track <- ten_track_data %>% select(ECR, YearPosted, MonthPosted) %>% 
  rowid_to_column() %>% 
  mutate(Date = paste(MonthPosted, " ", YearPosted))

#A
#Pearson's chi-squared test w/ bonferroni correction

ten_track_count <- table(stats_ten_track$YearPosted, stats_ten_track$MonthPosted)

ten_track_chi <- chisq.test(ten_track_count)

ten_track_posthoc <- chisq.posthoc.test(ten_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  spread(key = Value, value = stat) %>% 
  filter(`p values` <= 0.5)



#B

ecr_track_count_data <- stats_ten_track %>% 
  filter(ECR == "Yes") 

ecr_track_count <- table(ecr_track_count_data$YearPosted, ecr_track_count_data$MonthPosted)

ecr_track_chi <- chisq.test(ecr_track_count)

ecr_track_posthoc <- chisq.posthoc.test(ecr_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020" | Dimension == "2021") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  spread(key = Value, value = stat) %>% 
  filter(`p values` <= 0.5)

#C

temp_track_count <- table(non_track_data$YearPosted, non_track_data$MonthPosted)

temp_track_chi <- chisq.test(temp_track_count)

temp_posthoc <- chisq.posthoc.test(temp_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020"| Dimension == "2021") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  spread(key = Value, value = stat) %>% 
  filter(`p values` <= 0.5)

#D

#data has an empirical density, fits the gamma distribution, uses the Kolmogorov-Smirnov stat (per fitdist) -- need to perform a glmm
#cube-root transformed data, one-way anova, with a tukey multiple comparisions of means

ecr_region_count <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  filter(US_region %not_in% c("Rocky Mountains", "Noncontiguous")) %>% 
  rowid_to_column("id") %>% as.tibble() %>% 
  dplyr::select(id, MonthPosted, US_region, YearPosted) %>% 
  #mutate(US_region = as.factor(US_region)) %>% 
  count(YearPosted, MonthPosted, US_region, .drop = FALSE) %>% 
  filter(!is.na(US_region)) %>% 
  mutate(n = n^(1/3))

region_list <- ecr_region_count %>% pull(US_region) %>% unique()

ecr_reg_anov_list <- map(region_list, function(x){
  
  df <- ecr_region_count %>% filter(US_region == x)
  anova <- aov(n ~ YearPosted, data = df)
  summarise <- summary(anova)
  tukey <- TukeyHSD(anova)
  stats <- list(summarise, tukey)
  return(stats)
})

ecr_midwest <- ecr_reg_anov_list[[1]][[1]][[1]][[5]][[1]]

ecr_southeast <- ecr_reg_anov_list[[4]][[1]][[1]][[5]][[1]]

ecr_southwest <- ecr_reg_anov_list[[5]][[1]][[1]][[5]][[1]]

#E

ecr_uni_count <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  filter(!is.na(PUI_RI)) %>% 
  rowid_to_column("id") %>% as.tibble() %>% 
  count(YearPosted, MonthPosted, PUI_RI, .drop = FALSE) %>% 
  filter(!is.na(PUI_RI)) %>% 
  mutate(n = n^(1/3))

uni_list <- c("PUI", "RI")

ecr_uni_anov_list <- map(uni_list, function(x){
  
  df <- ecr_uni_count %>% filter(PUI_RI == x)
  anova <- aov(n ~ YearPosted, data = df)
  summarise <- summary(anova)
  tukey <- TukeyHSD(anova)
  stats <- list(summarise, tukey)
  return(stats)
})

ecr_pui <- ecr_uni_anov_list[[1]][[2]][[1]]%>% as_tibble(., rownames = "year") 

ecr_ri <- ecr_uni_anov_list[[2]][[1]][[1]][[5]][[1]]

#Figure 3----

#Fig A
a_count <- Fig_3A_data %>% select(research_category, false, true) %>% 
  remove_rownames %>% column_to_rownames(var="research_category") 

a_chi <- fisher.test(a_count)

#Fig B
b_count <- Fig_3B_data %>% select(first_gen_phd, false, true) %>% 
  remove_rownames %>% column_to_rownames(var="first_gen_phd")

b_chi <- fisher.test(b_count)

#Fig C
c_topic_count <- Fig_3c_data %>% filter(concern == "Added pandemic-related topics") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="research_category")

c_remote_count <- Fig_3c_data %>% filter(concern == "Be more 'remote-friendly'") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="research_category")

c_topic_chi <- fisher.test(c_topic_count)

c_remote_chi <- fisher.test(c_remote_count)

#Fig D
d_topic_count <- Fig_3d_data %>% filter(concern == "Added pandemic-related topics") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="desired_institution")

d_remote_count <- Fig_3d_data %>% filter(concern == "Be more 'remote-friendly'") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="desired_institution")
  
d_teach_count <- Fig_3d_data %>% filter(concern == "More online teaching practices") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="desired_institution")
  
d_topic_chi <- fisher.test(d_topic_count)

d_remote_chi <- fisher.test(d_remote_count)

d_teach_chi <- fisher.test(d_teach_count)

#Fig F
f_count <- strategy_data %>% select(id, current_app) %>% distinct() %>% 
  filter(!is.na(current_app)) %>% count(current_app) %>% pull(n)

f_chi <- chisq.test(f_count, p = c(0.33, 0.33, 0.34))