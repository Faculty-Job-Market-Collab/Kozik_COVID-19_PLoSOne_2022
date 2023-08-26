library(chisq.posthoc.test)
#Figure 3. What has happened to the job ads?

stats_ten_track <- ten_track_data %>% 
  select(ECR, YearPosted, MonthPosted) %>% 
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
  gather(`2018`:`2022`, key = YearPosted, value = n) %>% 
  #mutate(Stdev = ifelse(YearPosted == "Average", Stdev, 0))
  left_join(., y=ten_track_posthoc, by = c("YearPosted" = "Dimension", "MonthPosted")) %>% 
  mutate(MonthPosted = factor(MonthPosted, levels = month_levels))


FigS1_A <- ten_track_avg %>% 
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
  gather(`2018`:`2022`, key = YearPosted, value = n) %>% 
  #mutate(Stdev = ifelse(YearPosted == "Average", Stdev, 0))
  left_join(., y=ecr_track_posthoc, by = c("YearPosted" = "Dimension", "MonthPosted")) %>% 
  mutate(MonthPosted = factor(MonthPosted, levels = month_levels))

FigS1_B <- ecr_avg %>% 
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
  gather(`2018`:`2022`, key = YearPosted, value = n) %>% 
  #mutate(Stdev = ifelse(YearPosted == "Average", Stdev, 0))
  left_join(., y=temp_posthoc, by = c("YearPosted" = "Dimension", "MonthPosted")) %>% 
  mutate(MonthPosted = factor(MonthPosted, levels = month_levels))


FigS1_C <- temp_summary %>% 
  filter(YearPosted != "Average") %>% 
  ggplot(., aes(x = MonthPosted, y = n, group = YearPosted, color = YearPosted, label = P_value)) + 
  geom_line(size = 2)+
  geom_text(size=10, nudge_y = 40, show.legend = FALSE)+
  #geom_pointrange(aes(ymin=n-Stdev, ymax=n+Stdev))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 5000))+
  scale_color_manual(values = cbPalette)+ #values = c("#999999", "#E69F00"), breaks = c("Average", "2020"))+
  labs(y = "\nTemporary^ Faculty\nPositions", x = "\nMonth of Job Posting", color = "Year",
       caption = "^Adjunct, fixed-term, and non-tenure-track lecturer or faculty")+
  my_theme_leg_horiz

#compile figure----
FigS1 <- plot_grid(FigS1_A, FigS1_B, FigS1_C,
                  labels = c('A', 'B', 'C'#, ''
                             ),
                  label_size = 18, nrow = 3)

ggsave("Supp_Fig_1.png", device = 'png', units = "in", scale = 1.75,
       path = 'figures', width = 7, height = 6.8, dpi = 600)
