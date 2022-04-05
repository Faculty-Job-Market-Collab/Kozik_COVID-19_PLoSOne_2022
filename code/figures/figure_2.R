#Figure 1. The Pandemic begins, mid-interview portion of the Faculty Job Search

# A/B. Offers Rescinded by field and gender----
# requires get_offers_data 
fig1A_data <- get_plot_summary(res_demo_data, "gender", "covid_offers_rescinded") 

fig1A <-fig1A_data %>% 
  ggplot(aes(x = gender, y=percent_res, fill = gender))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  geom_hline(yintercept = percent_rescinded, linetype = "dashed", color = "black")+
  geom_text(aes(1, percent_rescinded+2, label = paste0(round(percent_rescinded, 1), "%")),
                color = "black")+
  labs(x = "\nReported Gender", y = "\nPercent of Offers Rescinded")+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  my_theme_horiz+
  right_margin

fig1B <- get_plot_summary(data = res_demo_data, 
                                   x = "research_category", y = "covid_offers_rescinded") %>% 
  ggplot(aes(x = research_category, y = percent_res, fill = research_category))+
  geom_col()+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(5, percent_rescinded+3, label = paste0(round(percent_rescinded, 1), "%")),
            color = "black")+
  labs(y = "\nPercent of Offers Rescinded", x = "\nReported Research Category")+
  scale_y_continuous(limits = c(0,40), expand = c(0,0))+
  my_theme

# C. Offers rescinded by race/ethnicity vs visa status----

#race_res_data <- race_data %>% 
#  group_by(spons_req, race_ethnicity, covid_offers_rescinded) %>% 
#  summarise(n = n()) %>% 
#  as_tibble() %>% 
#  spread(key = covid_offers_rescinded, value = n) %>% 
#  mutate(total = true + false,
#         total = if_else(is.na(total), "0", as.character(total)),
#         percent = get_percent(true, total),
#         race_ethnicity = paste0(race_ethnicity, "\n(n=", total, ")"),) %>% 
#  filter(total != 0) 

fig1C <- get_plot_summary(data = race_data, x = "race_ethnicity", y = "covid_offers_rescinded") %>% 
  ggplot(aes(x = fct_reorder(race_ethnicity, desc(percent_res)), y = percent_res, fill = race_ethnicity))+
  geom_col()+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  coord_flip(ylim = c(0,50))+
  scale_fill_manual(values = cbPalette)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(5, percent_rescinded+1.5, label = paste0(round(percent_rescinded, 1), "%")),
            color = "black")+
  #facet_wrap(~legal_status, ncol = 1, scales = "free_y")+
  labs(y = "\nPercent of Offers Rescinded", x = "\nReported Race/Ethnicity")+
  scale_y_continuous(expand = c(0,0))+
  my_theme_horiz+
  right_margin

#get_rescinded_inst <- function(x, df){
#  
#  #df <- rescinded_df
#  
#  id_x <- as.numeric(x)
#  
#  ea_id <- df %>% filter(id == id_x)
#  
#  one_offer <- ea_id %>% 
#    head(n = 1) 
#  
#  return(one_offer)
#}

#D. Compare the % of R1 vs PUI offers made vs offers rescinded----

fig1D <- PUI_RI_rescinded %>% 
  ggplot(aes(x = PUI_RI, y = percent_res, fill = PUI_RI))+
  geom_col()+
  scale_fill_manual(values = cbPalette)+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), vjust = -1.25)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(1, percent_rescinded+1, label = paste0(round(percent_rescinded, 1), "%")),
            color = "black")+
  #coord_flip(ylim = c(0,50))+
  #facet_wrap(~legal_status, ncol = 1, scales = "free_y")+
  labs(y = "\nPercent of Offers Rescinded\n", x = "\nInstitution Type")+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  my_theme_horiz
  
  #map_df(.x = id_list, 
  #                   .f = get_rescinded_inst, rescinded_df)

# E. Compare world region of institutions applied to and the number of offers rescinded----

#offers_world_region <- offers_df %>% 
#  filter(!is.na(world_region)) %>% 
#select(-inst_type, -covid_offers_rescinded) %>% 
#  group_by(world_region) %>% 
#  summarise(n_offers = n())
#
#rescinded_world_region <- rescinded_df %>% 
#  filter(!is.na(world_region)) %>% 
#  group_by(world_region, covid_offers_rescinded) %>% summarise(n = n()) %>% 
#  group_by(world_region) %>% summarise(n_rescinded = sum(n)) %>% as.tibble() 
#
#per_world_region_rescinded <- full_join(offers_world_region, rescinded_world_region, by = "world_region") %>% 
#  mutate(n_rescinded = replace(n_rescinded, is.na(n_rescinded), 0),
#         percent_res = get_percent(n_rescinded, n_offers),
#         world_region = paste0(world_region, "\n(n=", n_offers, ")"))
#
#fig1E <- per_world_region_rescinded %>% 
#  ggplot()+
#  geom_col(aes(x = fct_reorder(world_region, desc(percent_res)), y = percent_res))+
#  coord_flip()+
#  labs(y = "\nPercent of Offers Rescinded", x = "\nWorld Region\n",
#       caption = "(n = total number of offers made)")+
#  scale_y_continuous(expand = c(0,0))+
#  my_theme_horiz

#F. Compare US region of institutions applied to and the number of offers rescinded----
fig1F <- per_US_region_rescinded %>% 
  ggplot(aes(x = US_region, y = percent_res, fill = US_region))+
  geom_col()+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(2, percent_rescinded+1, label = paste0(round(percent_rescinded, 1), "%")),
            color = "black")+
  labs(y = "\nPercent of Offers Rescinded", x = "\nUS Region")+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  my_theme_horiz+
  right_margin

#build figure 1----
fig1AB <- plot_grid(fig1A, fig1B, labels = c('A', 'B'),
                    label_size = 18,
                    nrow = 1, rel_widths = c(.75, 1))

fig1DE <- plot_grid(fig1D, fig1F, labels = c('D', 'E'),
                    label_size = 18,
                    nrow = 1, rel_widths = c(.5, 1))

plot_grid(fig1AB, fig1C, fig1DE,
          labels = c('', 'C', ''),
          label_size = 18, rel_heights = c(1, 1.25, 1),
          nrow = 3)

ggsave("Figure_2.png", device = 'png', units = "in", scale = 1.75,
       path = 'figures', width = 7.5, height = 6.8)
