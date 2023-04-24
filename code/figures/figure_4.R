# Figure 4. Alternate strategies for the job search

#setup for A &B ----
#source("code/get_strategy_data.R")

# A. March (full survey) change in strategy by research field----
Fig_4A_data <- strategy_demo_data %>% 
  filter(survey_year == "2019-2020")

Fig_4A_data <- get_plot_summary(Fig_4A_data, "research_category", "covid_alter") %>% 
  filter(str_detect(research_category, "Integrated Sciences|NA") == FALSE)

Fig_4A_avg <- get_percent(sum(Fig_4A_data$true), sum(Fig_4A_data$n))
  
Fig_4A <- Fig_4A_data %>% 
  ggplot()+
  geom_col(aes(x = research_category, y=percent_res, fill = research_category))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  geom_hline(yintercept = Fig_4A_avg, linetype="dashed", color = "black")+
  geom_text(aes(3, Fig_4A_avg+2, label = Fig_4A_avg),  color = "black")+
  labs(y = "Early Pandemic Respondents That Altered Their\nResearch Statement to Focus on Remote or Computational Research (%)", x = "\nResearch Category")+
  my_theme_horiz+
  right_margin

#C. Late pandemic (full survey) change in strategy by research field----
Fig_4C_data <- strategy_demo_data %>% 
  filter(survey_year != "2019-2020")

Fig_4C_data <- get_plot_summary(Fig_4C_data, "research_category", "covid_alter") %>% 
  filter(str_detect(research_category, "Integrated Sciences|NA") == FALSE)

Fig_4C_avg <- get_percent(sum(Fig_4C_data$true), sum(Fig_4C_data$n))

Fig_4C <- Fig_4C_data %>% 
  ggplot()+
  geom_col(aes(x = research_category, y=percent_res, fill = research_category))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  geom_hline(yintercept = Fig_4C_avg, linetype="dashed", color = "black")+
  geom_text(aes(2, Fig_4C_avg+2, label = Fig_4C_avg),  color = "black")+
  labs(y = "Late Pandemic Respondents That Altered Their\nResearch Statement to Focus on Remote or Computational Research (%)", x = "\nResearch Category")+
  my_theme_horiz+
  right_margin

# D. March change in strategy by 1st gen PhD----
Fig_4D_data <- strategy_demo_data %>% 
  filter(survey_year == "2019-2020")

Fig_4D_data <- get_plot_summary(Fig_4D_data, "first_gen_phd", "covid_alter") %>% 
  filter(str_detect(first_gen_phd, "n=0") == FALSE) %>% 
  filter(false != 0)

Fig_4D_avg <- get_percent(sum(Fig_4D_data$true), sum(Fig_4D_data$n))

Fig_4D <- Fig_4D_data %>% 
  ggplot()+
  geom_col(aes(x = first_gen_phd, y=percent_res, fill = first_gen_phd))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  geom_hline(yintercept = Fig_4D_avg, linetype="dashed", color = "black")+
  geom_text(aes(1, Fig_4D_avg+1.25, label = Fig_4D_avg),  color = "black")+
  labs(y = "Early Pandemic Respondents That\nAltered Their Research Statement\nto Focus on Remote or\nComputational Research (%)", x = "\nFirst-Generation PhD Status\n")+
  my_theme_horiz+
  right_margin

# E. Late Pandemic change in strategy by 1st gen PhD (full survey)----
Fig_4E_data <- strategy_demo_data %>% 
  filter(survey_year != "2019-2020") %>% 
  filter(first_gen_phd != "Unsure")

Fig_4E_data <- get_plot_summary(Fig_4E_data, "first_gen_phd", "covid_alter") %>% 
  filter(str_detect(first_gen_phd, "n=0") == FALSE) %>% 
  filter(false != 0)

Fig_4E_avg <- get_percent(sum(Fig_4E_data$true), sum(Fig_4E_data$n))

Fig_4E <- Fig_4E_data %>% 
  ggplot()+
  geom_col(aes(x = first_gen_phd, y = percent_res, fill = first_gen_phd))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,25), expand = c(0,0))+
  geom_hline(yintercept = Fig_4E_avg, linetype="dashed", color = "black")+
  geom_text(aes(2, Fig_4E_avg+2, label = Fig_4E_avg),  color = "black")+
  labs(y = "Late Pandemic Respondents That\nAltered Their Research Statement\nto Focus on Remote or\nComputational Research (%)", x = "\nFirst-Generation PhD Status\n")+
  my_theme_horiz+
  right_margin

#Setup for B, E, & F----
#source("code/get_search_strategy_results.R")

alt_list <- c("Somewhat agree", "Strongly agree", "Agree")

bfg_data <- mid_pan_strategy_data %>% 
  filter(current_app == "Yes") %>% 
  filter(str_detect(concern, "altered") == TRUE) %>% 
  select(-current_app, -reasons) %>% 
  mutate(alt_strategy = if_else(extent %in% alt_list, "yes", "no"),
         concern = fct_recode(concern,
                              "Added pandemic-related topics" = "altered_research_covid",
                              "Be more 'remote-friendly'" = "altered_research_remote",
                              "More online teaching practices" = "altered_teaching_online"))

#n_res <- nrow(c_d_data)

# B. Fall (Mid-pandemic) alternate research &/or teaching strategies by research category----
Fig_4B_data <- bfg_data %>% 
  filter(concern != "More online teaching practices") %>% 
  select(id, research_category, concern, alt_strategy) %>% 
  distinct() %>% 
  count(research_category, concern, alt_strategy) %>% 
  spread(alt_strategy, n) %>% 
  mutate(no = coalesce(no, 0),
         yes = coalesce(yes, 0),
         per_yes = get_percent(yes, (yes+no)),
         research_category = paste0(research_category, " (n=", no+yes, ")"))

Fig_4B_avg <- Fig_4B_data %>% group_by(concern) %>% 
  summarise(no = sum(no), yes = sum(yes)) %>% as_tibble() %>% 
  mutate(research_category = "Group Average",
         per_yes = get_percent(yes, yes+no))

Fig_4B <- Fig_4B_data %>% 
  ggplot()+
  geom_col(aes(x = research_category, y=per_yes, fill = research_category))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  geom_hline(data = Fig_4B_avg, mapping = aes(yintercept = per_yes), 
             linetype="dashed", color = "black") +
  geom_text(data = Fig_4B_avg, mapping = aes(2, per_yes+10, label = per_yes),  color = "black")+
  facet_wrap(~concern)+
  scale_y_continuous(limits = c(0,50), expand = c(0,0))+
  labs(y = "Mid-Pandemic Respondents That\nAltered Their Research Statement (%)", x = "\nResearch Category")+
  my_theme_horiz+
  theme(panel.spacing = unit(2, "lines"))+
  right_margin

#F. Mid-pandemic/Fall alt research &/or teaching strategies according to institution type (PUI vs RI)----
Fig_4F_data <- bfg_data %>% 
  select(id, desired_institution, concern, alt_strategy) %>% 
  distinct() %>% 
  count(desired_institution, concern, alt_strategy) %>% 
  spread(alt_strategy, n) %>% 
  mutate(no = coalesce(no, 0),
         yes = coalesce(yes, 0),
         per_yes = get_percent(yes, (yes+no)),
         desired_institution = fct_recode(desired_institution,
                                          "RI" = "Research Intensive",
                                          "PUI" = "Primarily Undergraduate Serving"),
         desired_institution = paste0(desired_institution, " (n=", no+yes, ")"))

Fig_4F_avg <- Fig_4F_data %>% group_by(concern) %>% 
  summarise(no = sum(no), yes = sum(yes)) %>% as_tibble() %>% 
  mutate(desired_institution = "Group Average",
         per_yes = get_percent(yes, yes+no))

Fig_4F <- Fig_4F_data %>% 
  ggplot()+
  geom_col(aes(x = desired_institution, y=per_yes, fill = desired_institution))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  geom_hline(data = Fig_4F_avg, mapping = aes(yintercept = per_yes), 
             linetype="dashed", color = "black") +
  geom_text(data = Fig_4F_avg, mapping = aes(2, per_yes+3, label = per_yes),  color = "black")+
  facet_wrap(~concern)+
  scale_y_continuous(limits = c(0,60), expand = c(0,0))+
  labs(y = "Mid-Pandemic Respondents That Altered\nTheir Research or Teaching Statements (%)", x = "\nDesired Institution Type\n")+
  my_theme_horiz+
  theme(panel.spacing = unit(1, "lines"))+
  right_margin

# G. Commitment to academic career in Mar vs % in fall----
G_data_early <- clean_data %>% 
  filter(survey_year == "2019-2020") %>% 
  #select(id, faculty_offers) %>% distinct() %>% 
  #right_join(., clean_data, by = "id") %>%
  filter(faculty_offers == 0 | is.na(faculty_offers)) %>% 
  select(id, faculty_offers, commitment_impact) %>% distinct() %>% 
  filter(!is.na(commitment_impact)) %>% 
  count(commitment_impact) %>% 
  mutate(percent = get_percent(n, sum(n)),
         commitment_impact = replace_na(commitment_impact, "No response"),
         commitment_impact = paste0(commitment_impact, " (n=", n, ")")) 

FigG_early <- G_data_early %>% 
  ggplot()+
  geom_col(aes(x = commitment_impact, y = percent, 
               fill = commitment_impact))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,50), expand = c(0,0))+
  labs(y = "Early Pandemic Respondents (%)*", 
       x = "\nCommitment to\nAttaining a Faculty Position\n",
       caption = "*who did not recieve a faculty offer")+
  my_theme_horiz

G_data_mid <- mid_pan_strategy_data %>% select(id, current_app) %>% distinct() %>% 
  filter(!is.na(current_app)) %>% 
  count(current_app) %>% 
  mutate(percent= get_percent(n, sum(n)),
         current_app = replace_na(current_app, "No response"),
         current_app = fct_recode(current_app,
                                  "No, I decided to look\noutside of academia" = "No, I decided to change career paths and look outside of academia",
                                  "No, I decided to wait" = "No, I decided to wait to a later time"),
         current_app = paste0(current_app , " (n=", n, ")"))

FigG_mid <- G_data_mid %>% 
  ggplot()+
  geom_col(aes(x = current_app, y = percent, fill = current_app))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,75), expand = c(0,0))+
  labs(y = "Mid-Pandemic Respondents (%)", 
       x = "\nSubmitting Faculty Position\nApplications for 2020/21 Cycle")+
  my_theme_horiz

G_data_late <- clean_data %>% 
  filter(survey_year != "2019-2020") %>% 
  #select(id, faculty_offers) %>% distinct() %>% 
  #right_join(., clean_data, by = "id") %>%
  filter(faculty_offers == 0 | is.na(faculty_offers)) %>% 
  select(id, faculty_offers, commitment_impact) %>% distinct() %>% 
  filter(!is.na(commitment_impact)) %>% 
  count(commitment_impact) %>% 
  mutate(percent = get_percent(n, sum(n)),
         commitment_impact = replace_na(commitment_impact, "No response"),
         commitment_impact = paste0(commitment_impact, " (n=", n, ")")) 

FigG_late <- G_data_late %>% 
  ggplot()+
  geom_col(aes(x = commitment_impact, y = percent,
               fill = commitment_impact))+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(limits = c(0,50), expand = c(0,0))+
  labs(y = "Late Pandemic Respondents (%)*", 
       x = "\nCommitment to\nAttaining a Faculty Position\n",
       caption = "*who did not recieve a faculty offer")+
  my_theme_horiz

#generate Fig 4-----

Fig_4abc <- plot_grid(Fig_4A, Fig_4B, Fig_4C, labels = c('A', 'B', 'C'),
                     label_size = 18, nrow = 3)

ggsave("Figure_4abc.png", device = 'png', units = "in",
       path = 'figures', width = 12, height = 10)

Fig_4de <- plot_grid(Fig_4D, Fig_4E, labels = c('D', 'E'),
                     label_size = 18, nrow = 2)

ggsave("Figure_4de.png", device = 'png', units = "in",
       path = 'figures', width = 8, height = 8)

Fig_4G <- plot_grid(FigG_early, FigG_mid, FigG_late,
                    nrow = 1)

Fig_4fg <- plot_grid(Fig_4F, Fig_4G, 
                     labels = c('F', 'G'), 
                     nrow = 2, label_size = 18)

ggsave("Figure_4fg.png", device = 'png', units = "in", 
       path = 'figures', width = 20, height = 8)
