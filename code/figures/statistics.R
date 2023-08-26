stats_demo_data <- res_demo_data %>% 
  select(id, covid_offers_rescinded, adjusted_gender, peer, research_category)

#Figure 2 -----

#Fig A
gen_chi <- fisher.test(table(stats_demo_data$adjusted_gender, 
                             stats_demo_data$covid_offers_rescinded))

#Fig B
cat_chi <- fisher.test(table(stats_demo_data$research_category,
                             stats_demo_data$covid_offers_rescinded))

#Fig C
re_chi <- fisher.test(table(stats_demo_data$peer, 
                            stats_demo_data$covid_offers_rescinded))

#Fig D
pui_count <- PUI_RI_rescinded %>% select(PUI_RI, n_rescinded, nr) %>% 
  remove_rownames %>% column_to_rownames(var="PUI_RI")

pui_chi <- fisher.test(pui_count)
#
##Fig F
us_reg_count <- per_US_region_rescinded %>% select(US_region, n_rescinded, nr) %>% 
  remove_rownames %>% column_to_rownames(var="US_region") 

us_reg_chi <- fisher.test(us_reg_count)

fig2_chi_list <- c("gen_chi", "cat_chi", 
              "re_chi", "pui_chi",
              "us_reg_chi")

fig2_plot_list <- c('2A', '2B',
               '2C', '2D', '2F')

fig2_chi_tbl_raw <- map2_df(fig2_chi_list, fig2_plot_list, get_wilcox_tbl) 

fig2_chi_tbl <- fig2_chi_tbl_raw %>% 
  spread(key = attribute, value = value)

write_csv(fig2_chi_tbl, file = paste0("figures/fig2abcdf_chi_stats_", 
                                      Sys.Date(),".csv"))


#Figure 3 ----

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
  filter(`p values` <= 0.5) %>% 
  mutate(figure = "3A")

#B
ecr_track_count_data <- stats_ten_track %>% 
  filter(ECR == "Yes") 

ecr_track_count <- table(ecr_track_count_data$YearPosted, ecr_track_count_data$MonthPosted)

ecr_track_chi <- chisq.test(ecr_track_count)

ecr_track_posthoc <- chisq.posthoc.test(ecr_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020" | Dimension == "2021") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  spread(key = Value, value = stat) %>% 
  filter(`p values` <= 0.5) %>% 
  mutate(figure = "3B")

#C

temp_track_count <- table(non_track_data$YearPosted, non_track_data$MonthPosted)

temp_track_chi <- chisq.test(temp_track_count)

temp_posthoc <- chisq.posthoc.test(temp_track_count, method = "bonferroni") %>% 
  filter(Dimension == "2020"| Dimension == "2021") %>% 
  gather(Jan:Dec, key = "MonthPosted", value = "stat") %>% 
  spread(key = Value, value = stat) %>% 
  filter(`p values` <= 0.5) %>% 
  mutate(figure = "3C")

#compile 3ABC tests
fig3_chi_list <- c("ten_track_chi", "ecr_track_chi", 
                   "temp_track_chi")

fig3_plot_list <- c('3A', '3B', '3C')

fig3_chi_tbl <- map2_dfc(fig3_chi_list, fig3_plot_list, get_wilcox_tbl) 

colnames(fig3_chi_tbl) <- c("attribute_3a", "value_3a", "figure_3a",
                            "attribute_3b", "value_3b", "figure_3b",
                            "attribute_3c", "value_3c", "figure_3c")


write_csv(fig3_chi_tbl, file = paste0("figures/fig3abc_chi_stats_", 
                                      Sys.Date(),".csv"))

fig3_posthoc_tbl <- rbind(ten_track_posthoc, ecr_track_posthoc, 
                          temp_posthoc)

write_csv(fig3_posthoc_tbl, file = paste0("figures/fig3abc_posthoc_stats_", 
                                          Sys.Date(),".csv"))

#D

#data has an empirical density, fits the gamma distribution, uses the Kolmogorov-Smirnov stat (per fitdist) -- need to perform a glmm
#cube-root transformed data, one-way anova, with a tukey multiple comparisions of means

#ecr_region_count <- ten_track_data %>% 
#  filter(ECR == "Yes") %>% 
#  filter(Country == "USA") %>% 
#  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
#  filter(US_region %not_in% c("Rocky Mountains", "Noncontiguous")) %>% 
#  rowid_to_column("id") %>% as.tibble() %>% 
#  dplyr::select(id, MonthPosted, US_region, YearPosted) %>% 
#  #mutate(US_region = as.factor(US_region)) %>% 
#  count(YearPosted, MonthPosted, US_region, .drop = FALSE) %>% 
#  filter(!is.na(US_region)) %>% 
#  mutate(n = n^(1/3))
#
#region_list <- ecr_region_count %>% pull(US_region) %>% unique()
#
#ecr_reg_anov_list <- map(region_list, function(x){
#  
#  df <- ecr_region_count %>% filter(US_region == x)
#  anova <- aov(n ~ YearPosted, data = df)
#  summarise <- summary(anova)
#  region <- paste(x, "Data: One-way ANOVA, with a Tukey multiple comparisions of means")
#  tukey <- TukeyHSD(anova)
#  stats <- list(region, summarise, tukey)
#  return(stats)
#})
#
#sink(paste0("figures/anova_stats_fig3D_", Sys.Date(),".txt"))
#
#print(ecr_reg_anov_list)
#
#sink()

#E
#ecr_uni_count <- ten_track_data %>% 
#  filter(ECR == "Yes") %>% 
#  filter(Country == "USA") %>% 
#  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
#  filter(!is.na(PUI_RI)) %>% 
#  rowid_to_column("id") %>% as.tibble() %>% 
#  count(YearPosted, MonthPosted, PUI_RI, .drop = FALSE) %>% 
#  filter(!is.na(PUI_RI)) %>% 
#  mutate(n = n^(1/3))
#
#uni_list <- c("PUI", "RI")
#
#ecr_uni_anov_list <- map(uni_list, function(x){
#  
#  df <- ecr_uni_count %>% filter(PUI_RI == x)
#  anova <- aov(n ~ YearPosted, data = df)
#  inst <- paste(x, "Data: One-way ANOVA, with a Tukey multiple comparisions of means")
#  summarise <- summary(anova)
#  tukey <- TukeyHSD(anova)
#  stats <- list(inst, summarise, tukey)
#  return(stats)
#})
#
#sink(paste0("figures/anova_stats_fig3E_", Sys.Date(),".txt"))

#print(ecr_uni_anov_list)

#sink()

#Figure 4----

#Fig A
a_count <- Fig_4A_data %>% select(research_category, false, true) %>% 
  remove_rownames %>% column_to_rownames(var="research_category") 

a_chi <- fisher.test(a_count)

#Fig B
b_topic_count <- Fig_4B_data %>% filter(concern == "Added pandemic-related topics") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="research_category")

b_remote_count <- Fig_4B_data %>% filter(concern == "Be more 'remote-friendly'") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="research_category")

b_topic_chi <- fisher.test(b_topic_count)

b_remote_chi <- fisher.test(b_remote_count)

#Fig C
c_count <- Fig_4C_data %>% select(research_category, false, true) %>% 
  remove_rownames %>% column_to_rownames(var="research_category") 

c_chi <- fisher.test(c_count)

#Fig D
d_count <- Fig_4D_data %>% select(first_gen_phd, false, true) %>% 
  remove_rownames %>% column_to_rownames(var="first_gen_phd")

d_chi <- fisher.test(d_count)

#Fig E
e_count <- Fig_4E_data %>% select(first_gen_phd, false, true) %>% 
  remove_rownames %>% column_to_rownames(var="first_gen_phd")

e_chi <- fisher.test(e_count)

#Fig F
f_topic_count <- Fig_4F_data %>% filter(concern == "Added pandemic-related topics") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="desired_institution")

f_remote_count <- Fig_4F_data %>% filter(concern == "Be more 'remote-friendly'") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="desired_institution")
  
f_teach_count <- Fig_4F_data %>% filter(concern == "More online teaching practices") %>% 
  select(-concern, -per_yes) %>% 
  remove_rownames %>% column_to_rownames(var="desired_institution")
  
f_topic_chi <- fisher.test(f_topic_count)

f_remote_chi <- fisher.test(f_remote_count)

f_teach_chi <- fisher.test(f_teach_count)

#Fig G
g_count_early <- G_data_early %>% pull(n)

g_chi_early <- chisq.test(g_count_early, p = c(0.33, 0.33, 0.34))

g_count_mid <- mid_pan_strategy_data %>% 
  select(id, current_app) %>% distinct() %>% 
  filter(!is.na(current_app)) %>% count(current_app) %>% pull(n)

g_chi_mid <- chisq.test(g_count_mid, p = c(0.33, 0.33, 0.34))

g_count_late <- G_data_late %>% pull(n)

g_chi_late <- chisq.test(g_count_late, p = c(0.33, 0.33, 0.34))

#fig 4 chi stats
fig4_chi_list <- c("a_chi", "b_topic_chi", "b_remote_chi", "c_chi",
                   "d_chi", "e_chi",
                   "f_topic_chi", "f_remote_chi", "f_teach_chi", 
                   "g_chi_early", "g_chi_mid", "g_chi_late")

fig4_plot_list <- c('4A', '4B pandemic topics', '4B remote research', '4C',
                    '4D early first gen', '4E late first gen',
                    '4F pandemic topics', '4F remote research', 
                    '4F online teaching', 
                    '4G early commitment', '4G mid commitment',
                    '4G late commitment')

fig4_chi_tbl <- map2_df(fig4_chi_list, fig4_plot_list, get_wilcox_tbl) %>% 
  spread(key = figure, value = value)


write_csv(fig4_chi_tbl, file = paste0("figures/fig4_fisher-chi_stats_", 
                                          Sys.Date(),".csv"))
