library(chisq.posthoc.test)
#Figure 3. What has happened to the job ads?

stats_ten_track <- ten_track_data %>% select(ECR, YearPosted, MonthPosted) %>% 
  rowid_to_column() %>% 
  mutate(Date = paste(MonthPosted, " ", YearPosted))

# A. All faculty job ads----
#Pearson's chi-squared test w/ bonferroni correction

ten_track_count <- table(stats_ten_track$YearPosted, stats_ten_track$MonthPosted)

ten_track_chi <- chisq.test(ten_track_count)

ten_track_posthoc <- chisq.posthoc.test(ten_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  mutate(Value = if_else(Value == "p values", "P_value", Value)) %>% 
  spread(key = Value, value = stat) %>% 
  filter(P_value <= 0.001) %>% 
  mutate(P_value = "*") %>% 
  select(-Residuals)

#plotting data
ten_track_avg <- stats_ten_track %>% 
  count(YearPosted, MonthPosted) %>% spread(YearPosted, n) %>% 
  #rowwise() %>% 
  #mutate(Average = round(mean(`2018`,`2019`,`2021`), digits = 2),
  #       Stdev = round(sd(c(`2018`, `2019`, `2021`)), digits = 2)) %>% 
  #select(-c(`2018`, `2019`, `2021`)) %>% 
  gather(`2018`:`2021`, key = YearPosted, value = n) %>% 
  #mutate(Stdev = ifelse(YearPosted == "Average", Stdev, 0))
  left_join(., y=ten_track_posthoc, by = c("YearPosted" = "Dimension", "MonthPosted")) %>% 
  mutate(MonthPosted = factor(MonthPosted, levels = month_levels))


Fig3_A <- ten_track_avg %>% 
  filter(YearPosted != "Average") %>% 
  ggplot(., aes(x = MonthPosted, y = n, group = YearPosted, 
                color = YearPosted, label = P_value)) + 
  geom_line(size = 2)+
  geom_text(size=10, nudge_y = 40, show.legend = FALSE)+
  #geom_pointrange(aes(ymin=n-Stdev, ymax=n+Stdev))+
  coord_cartesian(ylim = c(0, 1750))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values = cbPalette)+ #values = c("#999999", "#E69F00"), breaks = c("Average", "2020"))+
  labs(y = "\nAll Tenure-Track\nPositions", x = "\nMonth of Job Posting", color = "Year")+
  my_theme_leg_horiz

# B. Assistant Tenure Track job ads----
ecr_track_count_data <- stats_ten_track %>% 
  filter(ECR == "Yes") 

ecr_track_count <- table(ecr_track_count_data$YearPosted, ecr_track_count_data$MonthPosted)

ecr_track_chi <- chisq.test(ecr_track_count)

ecr_track_posthoc <- chisq.posthoc.test(ecr_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  mutate(Value = if_else(Value == "p values", "P_value", Value)) %>% 
  spread(key = Value, value = stat) %>% 
  filter(P_value <= 0.001) %>% 
  mutate(P_value = "*") %>% 
  select(-Residuals)

ecr_avg <- ecr_summary %>% spread(YearPosted, n) %>% 
  #rowwise() %>% 
  #mutate(Average = round(mean(`2018`,`2019`,`2021`), digits = 2),
  #       Stdev = round(sd(c(`2018`, `2019`, `2021`)), digits = 2)) %>% 
  #select(-c(`2018`, `2019`, `2021`)) %>% 
  gather(`2018`:`2021`, key = YearPosted, value = n) %>% 
  #mutate(Stdev = ifelse(YearPosted == "Average", Stdev, 0))
  left_join(., y=ecr_track_posthoc, by = c("YearPosted" = "Dimension", "MonthPosted")) %>% 
  mutate(MonthPosted = factor(MonthPosted, levels = month_levels))

Fig3_B <- ecr_avg %>% 
  filter(YearPosted != "Average") %>% 
  ggplot(., aes(x = MonthPosted, y = n, group = YearPosted, color = YearPosted, label = P_value)) + 
  geom_line(size = 2)+
  geom_text(size=10, nudge_y = 40, show.legend = FALSE)+
  #geom_pointrange(aes(ymin=n-Stdev, ymax=n+Stdev))+
  coord_cartesian(ylim = c(0, 1750))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values = cbPalette)+ #values = c("#999999", "#E69F00"), breaks = c("Average", "2020"))+
  labs(y = "\nAssistant Professor\nTenure-Track Positions", x = "\nMonth of Job Posting", color = "Year")+
  my_theme_leg_horiz

# C. Temporary positions (define)----
temp_track_count <- table(non_track_data$YearPosted, non_track_data$MonthPosted)

temp_track_chi <- chisq.test(temp_track_count)

temp_posthoc <- chisq.posthoc.test(temp_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  mutate(Value = if_else(Value == "p values", "P_value", Value)) %>% 
  spread(key = Value, value = stat) %>% 
  filter(P_value <= 0.001) %>% 
  mutate(P_value = "*") %>% 
  select(-Residuals)

temp_summary <- non_track_data %>% 
  count(YearPosted, MonthPosted) %>% spread(YearPosted, n) %>% 
  #rowwise() %>% 
  #mutate(Average = round(mean(`2018`,`2019`,`2021`), digits = 2),
  #       Stdev = round(sd(c(`2018`, `2019`, `2021`)), digits = 2)) %>% 
  #select(-c(`2018`, `2019`, `2021`)) %>% 
  gather(`2018`:`2021`, key = YearPosted, value = n) %>% 
  #mutate(Stdev = ifelse(YearPosted == "Average", Stdev, 0))
  left_join(., y=temp_posthoc, by = c("YearPosted" = "Dimension", "MonthPosted")) %>% 
  mutate(MonthPosted = factor(MonthPosted, levels = month_levels))


Fig3_C <- temp_summary %>% 
  filter(YearPosted != "Average") %>% 
  ggplot(., aes(x = MonthPosted, y = n, group = YearPosted, color = YearPosted, label = P_value)) + 
  geom_line(size = 2)+
  geom_text(size=10, nudge_y = 40, show.legend = FALSE)+
  #geom_pointrange(aes(ymin=n-Stdev, ymax=n+Stdev))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 3500))+
  scale_color_manual(values = cbPalette)+ #values = c("#999999", "#E69F00"), breaks = c("Average", "2020"))+
  labs(y = "\nTemporary^ Faculty\nPositions", x = "\nMonth of Job Posting", color = "Year",
       caption = "^Adjunct, fixed-term, and non-tenure-track lecturer or faculty")+
  my_theme_leg_horiz

# D. Region where jobs are available vs. outbreak?----

ecr_reg_stats <- tibble(Year = c("2018"),
                        US_region = c("Southeast", "Southwest", "Midwest"),
                        P_value = c("#"))

ecr_region_fig_data <- ecr_region_summary %>% 
  filter(Year != "Average") %>% 
  left_join(., y=ecr_reg_stats, by = c("US_region", "Year")) %>% 
  mutate(US_region = as.factor(US_region),
         Year = factor(Year, levels = c("2018", "2019", "2020", "2021")))

Fig3_D <- ggplot(ecr_region_fig_data, aes(x = US_region, y = Percent, 
                                          fill = Year, label = P_value))+
  geom_col(position = "dodge")+
  scale_y_continuous(expand = c(0,0))+
  geom_text(size = 6, nudge_y = 3)+
  geom_segment(x = .5,y = 25, yend = 25, xend = 1.5)+
  geom_segment(x = 5.5,y = 23, yend = 23, xend = 6.5)+
  geom_segment(x = 6.5,y = 5, yend = 5, xend = 7.5)+
  scale_fill_manual(values = cbPalette)+#values = c("#E69F00", "#56B4E9", "#009E73"), breaks = c("2019", "2020", "2021"))+
  coord_flip()+
  labs(y = "% Assistant Professor Tenure-Track\nPositions per Cycle (June - Dec)", 
       x = "\nUS Region of the\n Posting Institution")+
  my_theme_horiz

# E. University type vs job availablity ----
ecr_uni_stats <- tibble(Year = c("2019"),
                        PUI_RI = c("RI"),
                        P_value = c("#"))

ecr_uni_fig_data <- ecr_uni_summary %>% 
  filter(Year != "Average") %>% 
  left_join(., y=ecr_uni_stats, by = c("PUI_RI", "Year")) 

Fig3_E <- ggplot(ecr_uni_fig_data, aes(x = fct_reorder(PUI_RI, Percent), 
                                   y = Percent, fill = Year,
                                   label = P_value))+
  geom_col(position = "dodge")+
  geom_text(size = 10, nudge_y = 6)+
  geom_segment(x = 1.5,y = 73, yend = 73, xend = 2.5)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 80))+
  scale_fill_manual(values = cbPalette)+ #c("#E69F00", "#56B4E9", "#009E73"), breaks = c("2019", "2020", "2021"))+
  coord_flip()+
  labs(y = "% Assistant Professor Tenure Track Positions\n per Cycle (June - Dec)", 
       x = "\nInstitution Type")+
  my_theme_leg_horiz

#Generate Figure 3----

Fig3_DE <- plot_grid(Fig3_D, Fig3_E, labels = c('D', 'E'),
                     label_size = 18, nrow = 1)

Fig3 <- plot_grid(Fig3_A, Fig3_B, Fig3_C, Fig3_DE,
                  labels = c('A', 'B', 'C', ''),
                  label_size = 18, nrow = 4)

ggsave("Figure_3.png", device = 'png', units = "in", scale = 1.75,
       path = 'figures', width = 7, height = 6.8)

#Relevant Fig 3 Data Calculations----
#ecr_region_month_data <- ten_track_data %>% 
#  filter(ECR == "Yes") %>% 
#  filter(YearPosted == "2020" & Country == "USA") %>% 
#  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
#  select(MonthPosted, US_region, NumPositions) %>% 
#  count(MonthPosted, US_region, NumPositions) %>% 
#  spread(NumPositions, n) %>% 
#  mutate(`2` = 2*`2`,
#         `2` = coalesce(`2`, 0),
#         `3` = 3*`3`,
#         `3` = coalesce(`3`, 0),
#         n = `2`+`3`+`<NA>`,
#         percent = get_percent(n, sum(n)),
#         US_region = paste0(US_region, " (n=", n, ")")) %>% 
#  select(MonthPosted, US_region, n, percent) %>% 
#  filter(percent >= 2) %>% 
#  arrange(desc(percent))
#
#temp_region_month_data <- non_track_data %>% 
#  filter(YearPosted == "2020" & Country == "USA") %>% 
#  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
#  select(MonthPosted, US_region, NumPositions) %>% 
#  count(MonthPosted, US_region, NumPositions) %>% 
#  spread(NumPositions, n) %>% select(-`100`, -`2020`) %>% 
#  mutate(`2` = 2*`2`,
#         `2` = coalesce(`2`, 0),
#         `3` = 3*`3`,
#         `3` = coalesce(`3`, 0),
#         `4` = 4*`4`,
#         `4` = coalesce(`4`, 0),
#         `6` = 6*`6`,
#         `6` = coalesce(`6`, 0),
#         `16` = 16*`16`,
#         `16` = coalesce(`16`, 0),
#         n = `2`+`3`+`4`+`6`+`16`+`<NA>`,
#         percent = get_percent(n, sum(n)),
#         US_region = paste0(US_region, " (n=", n, ")")) %>% 
#  select(MonthPosted, US_region, n, percent) %>% 
#  filter(percent >= 3) %>% 
#  arrange(desc(percent))
#
#temp_inst_data <- non_track_data %>% 
#  filter(YearPosted == "2020" & Country == "USA") %>% 
#  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
#  select(PUI_RI, NumPositions) %>% 
#  count(PUI_RI, NumPositions) %>% 
#  spread(NumPositions, n) %>% select(-`100`, -`2020`) %>% 
#  filter(!is.na(PUI_RI)) %>% 
#  mutate(`2` = 2*`2`,
#         `2` = coalesce(`2`, 0),
#         `3` = 3*`3`,
#         `3` = coalesce(`3`, 0),
#         `4` = 4*`4`,
#         `4` = coalesce(`4`, 0),
#         `6` = 6*`6`,
#         `6` = coalesce(`6`, 0),
#         `16` = 16*`16`,
#         `16` = coalesce(`16`, 0),
#         n = `2`+`3`+`4`+`6`+`16`+`<NA>`,
#         percent = get_percent(n, sum(n))) %>% 
#  select(PUI_RI, n, percent)
#