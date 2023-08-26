#Figure 2. The Pandemic begins, mid-interview portion of the Faculty Job Search
fig2_data <- res_demo_data %>% 
  filter(survey_year == "2019-2020")

# A/B. Offers Rescinded by field and gender----
# requires get_3yr_survey_results
fig2A_data <- get_plot_summary(fig2_data, "adjusted_gender", 
                               "covid_offers_rescinded") 

fig2A <- fig2A_data %>% 
  ggplot(aes(x = adjusted_gender, y=percent_res, 
             fill = adjusted_gender))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  geom_hline(yintercept = percent_rescinded, 
             linetype = "dashed", color = "black")+
  geom_text(aes(1, percent_rescinded+3, 
                label = paste0(percent_rescinded, "%")),
                color = "black")+
  labs(x = "\nReported Gender", y = "Percent of Offers Rescinded")+
  scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  my_theme_horiz+
  right_margin

# B. Offers rescinded by race/ethnicity----
fig2B_data <- get_plot_summary(data = fig2_data, 
                          x = "peer", y = "covid_offers_rescinded")

fig2B <- fig2B_data %>% 
  filter(peer != "NR (0/1)") %>% 
  ggplot(aes(x = fct_reorder(peer, desc(percent_res)), y = percent_res, 
             fill = peer))+
  geom_col()+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  coord_flip(ylim = c(0,20))+
  scale_fill_manual(values = cbPalette)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(2, percent_rescinded+2, 
                label = paste0(percent_rescinded, "%")),
            color = "black")+
  #facet_wrap(~legal_status, ncol = 1, scales = "free_y")+
  labs(y = "Percent of Offers Rescinded", x = "\nPEER Status")+
  scale_y_continuous(expand = c(0,0))+
  my_theme_horiz+
  right_margin

# C. Offers rescinded by research category----
fig2C_data <- get_plot_summary(data = fig2_data, 
                                   x = "research_category", y = "covid_offers_rescinded")

fig2C <- fig2C_data %>% 
  ggplot(aes(x = research_category, y = percent_res, fill = research_category))+
  geom_col()+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(5, percent_rescinded+1, 
                label = paste0(percent_rescinded, "%")),
            color = "black")+
  labs(y = "Percent of Offers Rescinded", x = "\nReported Research Category")+
  scale_y_continuous(limits = c(0,25), expand = c(0,0))+
  my_theme

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
fig2D <- PUI_RI_rescinded %>% 
  ggplot(aes(x = PUI_RI, y = percent_res, fill = PUI_RI))+
  geom_col()+
  scale_fill_manual(values = cbPalette)+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), vjust = -1.25)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(1, percent_rescinded+7, label = paste0(percent_rescinded, "%")),
            color = "black")+
  coord_flip()+
  #facet_wrap(~legal_status, ncol = 1, scales = "free_y")+
  labs(y = "Percent of Offers Rescinded", x = "\nInstitution Type")+
  #scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  my_theme_horiz
  
  #map_df(.x = id_list, 
  #                   .f = get_rescinded_inst, rescinded_df)

#E. Compare US region of institutions applied to and the number of offers rescinded----
fig2E <- per_US_region_rescinded %>% 
  ggplot(aes(x = US_region, y = percent_res, fill = US_region))+
  geom_col()+
  #geom_text(aes(label = r), position = position_dodge(width = 0.9), hjust = -0.25)+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  geom_hline(yintercept = percent_rescinded, linetype="dashed", color = "black")+
  geom_text(aes(2, percent_rescinded+10, label = paste0(round(percent_rescinded, 1), "%")),
            color = "black")+
  labs(y = "Percent of Offers Rescinded", x = "\nUS Region")+
  #scale_y_continuous(limits = c(0,20), expand = c(0,0))+
  my_theme_horiz+
  right_margin

#build figure 2----
fig2AB <- plot_grid(fig2A, fig2B, labels = c('A', 'B'),
                    label_size = 18,
                    nrow = 1, rel_widths = c(.75, 1))

fig2DE <- plot_grid(fig2D, fig2E, labels = c('D', 'E'),
                    label_size = 18,
                    nrow = 1, rel_widths = c(.5, 1))

plot_grid(fig2AB, fig2C, fig2DE,
          labels = c('', 'C', ''),
          label_size = 18, #rel_heights = c(1, 1.25),
          nrow = 3)

ggsave("Figure_2.png", device = 'png', units = "in", scale = 1.75,
       path = 'figures', width = 7.5, height = 6.8, dpi = 600)
