library(scales)
library(RColorBrewer)
library(cowplot)
library(grid)
#library(patchwork)

#preferred themes----
my_theme <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_horiz <- theme_classic()+ 
  theme(legend.position = "none", axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        plot.caption = element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5),
        strip.text = element_text(size = 14),
        panel.border = element_blank())

my_theme_leg <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_left <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left", plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_horiz <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        plot.subtitle = element_text(size = 12, hjust = .5),
        strip.text = element_text(size = 14),
        panel.border = element_blank())

right_margin <- theme(plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5, unit = "pt"))

#adding proportion/count labels to barchart
prop_lab_low <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = 1)

count_lab_low <- geom_text(stat = "count", aes(label=..count..), vjust = 1)

prop_lab_high <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = -1)

count_lab_high <- geom_text(stat = "count", aes(label=..count..), vjust = -1)

#colorblind palettes
#The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #goldenrod, light blue, dark green, grey, gold, navy blue, orange rust, pink

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #black

#plotting placeholder----
blank_plot <- ggplot()+
  theme_classic()+theme(line = element_blank())

blank <- plot_grid(blank_plot)

#plotting functions----

#plot feature weights
feature_box_plot <- function(df){
  plot <- ggplot(df)+
    geom_boxplot(aes(x = clean_feat, y = weight))+
    coord_flip()+
    labs(x = "\nLogistic Regression Variables", y = "Weight (Absolute value)")+
    my_theme_horiz
  
  return(plot)
}
