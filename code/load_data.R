library(tidyverse) #for data wrangling
library(data.table) #for setnames()

source("code/analysis_functions.R")
source("code/get_plot_options.R")

#HERC data----
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

herc_data <- read_csv("data/final_herc_data_18-22_2023-08-10.csv") %>% 
  mutate(YearPosted = as_factor(YearPosted),
         MonthPosted = factor(MonthPosted, levels = month_levels))

#survey data----
survey_data_3yrs <- read_csv("data/covid_survey_data_19-22.csv") #load data

## question-based datasets----
clean_data <- survey_data_3yrs %>% 
  mutate(research_category = fct_collapse(research_category,
                                          "Mathematics & Engineering Sciences" = c("Mathematical & Physical Sciences",
                                                                                   "Engineering", "Computer & Information Sciences"),
                                          "Social & Behavioral Sciences" = c("Humanities", "Social, Behavior, & Economic Sciences"))
         )

offers_df <- read_csv("data/carn_offer_data.csv")