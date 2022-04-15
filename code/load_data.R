library(tidyverse) #for data wrangling
library(data.table) #for setnames()

source("code/analysis_functions.R")
source("code/get_plot_options.R")

#HERC data----

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

herc_data <- read_csv("data/herc_dataset.csv") %>% 
  mutate(YearPosted = as_factor(YearPosted),
         MonthPosted = factor(MonthPosted, levels = month_levels))

#survey data----

clean_data <- read_csv("data/survey_covid_dataset.csv") #load data


## question-based datasets----
demographics <- select(clean_data, id, gender, research_category, race_ethnicity, first_gen_phd) %>% 
  mutate(gender = if_else(gender=="Non-binary"|gender=="Unlisted gender"|is.na(gender), "Gender minority", gender),
         research_category = fct_collapse(research_category,
                                          "Mathematics & Engineering Sciences" = c("Mathematical & Physical Sciences",
                                                                                   "Engineering", "Computer & Information Sciences"),
                                          "Social & Behavioral Sciences" = c("Humanities", "Social, Behavior, & Economic Sciences"))
         )

covid_only <- select(clean_data, contains("covid"), id)

offers_df <- read_csv("data/carn_offer_data.csv")