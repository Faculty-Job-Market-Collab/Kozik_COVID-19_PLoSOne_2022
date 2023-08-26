#get list of states & regions
us_regions <- read_csv("data/us_region_list.csv")

#read in cleaned carnegie data----
clean_carn_data <- read_csv("data/clean_carnegie_data.csv") %>% 
  select(NAME, STABBR, DOCRSDEG, HBCU, HSI, MSI, `S&ER&D`, 
         SOCSC_RSD, STEM_RSD, LANDGRNT,	MEDICAL,	TRIBAL, WOMENS)

#determine r1 vs pui status----
pui_carn <- clean_carn_data %>% 
  mutate(PUI_RI = if_else(DOCRSDEG >= 21, "RI", "PUI"))

#merge region to carn data ----
reg_carn <- left_join(pui_carn, us_regions, 
                      by = c("STABBR" = "State_abbvr")) %>% 
  distinct()

#source non-carn universities/institutions
non_carn_unis <- read_csv("data/non-carnegie_unis.csv") %>% ## Need to work on matching inst names for non-carn inst
  mutate(Institution = str_to_title(Institution),
         Institution = str_remove_all(Institution, "'"),
         `Full Institution Name` = str_remove_all(`Full Institution Name`, "'"),
         `Full Institution Name` = str_to_title(`Full Institution Name`))

#get nonUS/nonCarn inst
fix_non_carn <- function(x){
  case_when(
    str_detect(x, "Calagary") ~ "University Of Calgary",
    str_detect(x, "Curie") ~ "Pierre And Marie Curie University",
    str_detect(x, "Z.rich|Zurich") ~ "University Of Zurich",
    str_detect(x, "Qub") ~ "Queens University Belfast",
    TRUE ~ x
  )
}
