#import uni list and uni data -- wrangle uni names to sort/join lists
# SETUP ------
library(tidyverse)
library(readxl)
library(fuzzyjoin)

source("code/univ_categories/wrangle_uni_functions.R")

#import carnegie data
carnegie_data <- read_excel("data/CCIHE2018-PublicData.xlsx", 
                            sheet = 4, col_names = TRUE) %>% 
  mutate(NAME = str_to_title(NAME),
         NAME = str_replace(NAME, "Saint |^St ", "St "),
         NAME = str_replace_all(NAME, "&", "And"),
         NAME = str_replace_all(NAME, "Aandm|AAndM", "A And M"),
         NAME = str_replace_all(NAME, "Aandt|AAndT", "A And T"),
         NAME = str_replace(NAME, " At | In |/", " "),
         NAME = str_replace_all(NAME, "-", " "),
         NAME = str_remove(NAME, "Penn State "),
         NAME = str_remove_all(NAME, "The |Campus"),
         NAME = map(NAME, replace_uny),
         NAME = unlist(NAME),
         NAME = str_squish(NAME),
         NAME = str_replace_all(NAME, ",|'|\\.", ""))

#clean Carnegie inst names
carnegie_inst_names <- carnegie_data %>% 
  select(NAME)

# import survey uni list
all_survey_inst <- clean_data %>% 
  select(id, contains("instit")) %>% 
  select(-num_institution_contacted) %>% 
  gather(on_site_institutions:postdoc_institution, key = "inst_type", value = "inst_name") %>% 
  mutate(inst_name = str_to_title(inst_name)) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", 
                     "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20"), 
           sep = ";", extra = "merge") %>% 
  gather(x1:x20, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na("inst_name") & str_detect(inst_name, "[[:digit:]]") == FALSE)

comma_drops <- c(9, 14, 20, 24, 32, 38, 46, 50, 53, 55:58, 61, 63, 64, 72, 75, 77, 78, 81:84, 87:89, 91, 93:98, 100, 102:104, 106:112)

comma_split_data <- all_survey_inst %>% 
  filter(str_count(inst_name, "Univ") >= 2 | str_detect(inst_name, "in Can|,") == TRUE) %>% 
  filter(inst_type != "phd_institution") %>% 
  slice(-comma_drops)

comma_split <- comma_split_data %>% 
  mutate(inst_name = str_replace(inst_name, "University - University", "University, University")
         ) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), 
                                                 sep = ",", extra = "merge") %>% 
  gather(x1:x8, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na(inst_name))

comma_split_row_list <- comma_split_data %>% 
  pull(inst_name)

#list of unis from survey data - separated into individual rows
all_institutions_split <- all_survey_inst %>% 
  filter(inst_name %not_in% comma_split_row_list) %>% 
  rbind(., comma_split)

#list of non-US inst
non_carn_inst <- c("Inc|Associat|Society|Hospital| Lab|Foundation")

drop_inst <- c("Northwestern|Cornell|Boston|Florida|Wesleyan|Georgetown|Northwest|Temple")

#list of colleges
mult_loc_college_last_list <- carnegie_inst_names %>%
  filter(str_detect(NAME, "College,")== TRUE) %>% 
  mutate(NAME = str_replace(NAME, " College", ""),
         NAME = str_replace(NAME, ",", ",?")
         ) %>% 
  pull(NAME) %>% str_c(collapse = "|")

mult_loc_coll_of_list <- carnegie_inst_names %>% 
  filter(str_detect(NAME, "^College Of") & str_detect(NAME, ",")) %>% 
  mutate(NAME = str_replace(NAME, "College Of", ""),
         NAME = str_replace(NAME, ",", ",?")
         ) %>% 
  pull(NAME) %>% str_c(collapse = "|")

state_college_list <- carnegie_inst_names %>% 
  filter(str_detect(NAME, "State College$") == TRUE) %>% 
  mutate(NAME = str_replace(NAME, "(?<=State) College$", "$"))

x_college_list <- carnegie_inst_names %>% 
  filter(str_count(NAME, " ")==1) %>% 
  filter(str_detect(NAME, "College") == TRUE)%>% 
  filter(str_detect(NAME, drop_inst) == FALSE) %>% 
  mutate(NAME = paste0("^", str_replace(NAME, " College", ""))) %>% 
  rbind(., state_college_list) %>% 
  pull(NAME) %>% unique() %>% str_c(collapse = "|")

#list of universities
mult_loc_uni_last_list <- carnegie_inst_names %>%
  filter(str_detect(NAME, "University,")== TRUE) %>% 
  mutate(NAME = str_replace(NAME, " University", ""),
         NAME = str_replace(NAME, ",", ",?")
         ) %>% 
  pull(NAME) %>% str_c(collapse = "|")

mult_loc_uni_of_list <- carnegie_inst_names %>% 
  filter(str_detect(NAME, "^University Of") & str_detect(NAME, ",")) %>% 
  mutate(NAME = str_replace(NAME, "University Of", ""),
         NAME = str_replace(NAME, ",", ",?")
         ) %>% 
  pull(NAME) %>% str_c(collapse = "|")

state_uni_list <- carnegie_inst_names %>% 
  filter(str_detect(NAME, "State University$") == TRUE) %>% 
  mutate(NAME = str_replace(NAME, "(?<=State) University$", "$"))

x_uni_list <- carnegie_inst_names %>% 
  filter(str_count(NAME, " ")==1) %>% 
  filter(str_detect(NAME, "University") == TRUE) %>% 
  filter(str_detect(NAME, drop_inst) == FALSE) %>% 
  mutate(NAME = paste0("^", str_replace(NAME, " University", ""))) %>% 
  rbind(., state_uni_list) %>% 
  pull(NAME) %>% str_c(collapse = "|")


#clean survey inst names
extend_surv_inst <- all_institutions_split %>% 
  #filter(str_detect(inst, drop_inst) == FALSE) %>% 
  mutate(inst_name = str_replace(inst_name, "U\\.? |U$|Univ\\.? |Univ$|Uni\\w*\\b|Uni |Unviersity", "University "),
         inst_name = str_replace(inst_name, "Saint |^St ", "St "),
         inst_name = str_replace_all(inst_name, pattern = "&", replacement = "And"),
         inst_name = str_replace_all(inst_name, "Aandm|AAndM", "A And M"),
         inst_name = str_replace_all(inst_name, "Aandt|AAndT", "A And T"),
         inst_name = str_remove_all(inst_name, ",|The |\\.|Campus|Tuscaloosa|\\(.+| (?=,)|'"),
         inst_name = str_replace_all(inst_name, pattern = " At | In |,, |/|-", replacement = " "),
         inst_name = str_replace(inst_name, " Or ", " Of "),
         inst_name = map(inst_name, replace_uny),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_inst_abbrv),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_inst_name),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_campus),
         inst_name = unlist(inst_name),
         inst_name = str_to_title(inst_name),
         inst_name = str_squish(inst_name)
         )

inst_join <- carnegie_inst_names %>% 
  mutate(NAME = str_remove_all(NAME, ",")) %>% 
  left_join(extend_surv_inst, ., by = c("inst_name" = "NAME"), keep = TRUE) %>% 
  distinct()

na_inst <- inst_join %>% 
  filter(is.na(NAME)) %>% 
  select(inst_name) %>% distinct()

matches <- inst_join %>% 
  filter(inst_name == NAME) %>% 
  distinct()

#get_matches <- function(x){
#  
#}

missing <- inst_join %>% 
  filter(is.na(NAME)) %>% 
  select(-NAME) %>% 
  mutate(inst_name = map(inst_name, replace_uny),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_inst_abbrv),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_inst_name),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_campus),
         inst_name = unlist(inst_name),
         inst_name = map(inst_name, fix_inst_name),
         inst_name = unlist(inst_name),
         inst_name = str_squish(inst_name),
         ) %>% 
  stringdist_left_join(., carnegie_inst_names, 
                       by = c("inst_name" = "NAME"), 
                       method = "lcs", max_dist = 6
  ) %>% 
  distinct()

slice_list <- c(3, 4, 31, 46, 47, 77, 88, 89, 117, 176, 303, 386, 466, 498, 499, 518, 519, 531, 582, 584, 594, 599, 600, 607, 608, 614, 615, 617, 638, 640, 641, 643, 644, 652:656, 665)

check_missing_matches <- missing %>% 
  filter(!is.na(NAME)) %>% 
  filter(inst_name != NAME) %>% 
  slice(slice_list)

final_missing <- missing %>% 
  filter(!is.na(NAME)) %>% 
  filter(inst_name != NAME) %>% 
  slice(-slice_list) %>% 
  select(inst_name) %>% distinct()

all_matches <- missing %>% 
  filter(inst_name == NAME) %>% 
  rbind(., matches, check_missing_matches)

write_csv(final_missing, "data/missing_unis.csv")

write_csv(all_matches, "data/carnegie_inst_matches.csv")

write_csv(carnegie_data, "data/clean_carnegie_data.csv")

write_csv(extend_surv_inst, "data/cleaned_survey_inst.csv")
