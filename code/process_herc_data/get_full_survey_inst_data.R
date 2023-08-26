#get list of states & regions
us_regions <- read_csv("data/us_region_list.csv")

AFR <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Ivory Coast", "Democratic Republic of the Congo", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Eswatini", "Togo", "Uganda", "Tanzania", "Zambia", "Zimbabwe")

AMR <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela")

SEAR <- c("Bangladesh", "Bhutan", "North Korea", "India", "Indonesia", "Maldives", "Myanmar", "Nepal", "Sri Lanka", "Thailand", "Timor-Leste")

EUR <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan")

EMR <- c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Pakistan", "Palestine", "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syria", "Tunisia", "United Arab Emirates", "Yemen")

WPR <- c("Australia", "Brunei", "Cambodia", "China", "Cook Islands", "Fiji", "Japan", "Kiribati", "Laos", "Malaysia", "Marshall Islands", "Micronesia", "Mongolia", "Nauru", "New Zealand", "Niue", "Palau", "Papua New Guinea", "Philippines", "Samoa", "Singapore", "Solomon Islands", "South Korea", "Taiwan", "Tonga", "Tuvalu", "Vanuatu", "Vietnam")

#FUNCTIONS -----
get_world_region <- function(x){
  case_when(
    x %in% AFR ~ "African",
    x %in% AMR ~ "The Americas",
    x %in% SEAR ~ "South-East Asian",
    x %in% EUR ~ "European",
    x %in% EMR ~ "Eastern Mediterranean",
    x %in% WPR ~ "Western Pacific",
    x == "Canada" | x == "canada" ~ "Canada",
    x == "USA" ~ "USA"
  )
}

#get nonUS/nonCarn inst
fix_non_carn <- function(x){
  case_when(
    str_detect(x, "Calagary") ~ "University Of Calgary",
    str_detect(x, "Curie") ~ "Pierre And Marie Curie University",
    str_detect(x, "Zürich|Zurich") ~ "University Of Zurich",
    str_detect(x, "Qub") ~ "Queens University Belfast",
    str_detect(x, "Max Planck") ~ "Max Planck Institute",
    str_detect(x, "Oklahoma Medical Research") ~ "Oklahoma Medical Research Foundation",
    str_detect(x, "Tifr") ~ "National Center For Biological Sciences University Tifr India",
    str_detect(x, "Scripps Research") ~ "Scripps Research Institute",
    str_detect(x, "Imp Vienna") ~ "Imp University Vienna",
    TRUE ~ x
  )
}

#read in cleaned carnegie data----
clean_carn_data <- read_csv("data/clean_carnegie_data.csv") %>% 
  select(NAME, STABBR, DOCRSDEG, HBCU, HSI, MSI, `S&ER&D`, 
         SOCSC_RSD, STEM_RSD, LANDGRNT,	MEDICAL,	TRIBAL, WOMENS)

all_matches <- read_csv("data/carnegie_inst_matches.csv")

clean_surv_inst <- read_csv("data/cleaned_survey_inst.csv")

#merge region to carn data & determine r1 vs pui status----
reg_carn <- left_join(clean_carn_data, us_regions, 
                      by = c("STABBR" = "State_abbvr")) %>% 
  #select(-STABBR) %>% 
  distinct() %>% 
  mutate(PUI_RI = if_else(DOCRSDEG >= 21, "RI", "PUI"))

carn_region_join <- all_matches %>% 
  select(-inst_name) %>% 
  left_join(., reg_carn, by = c("NAME")) %>% 
  rename(US_region = Region, State_Providence = State_name) %>% 
  mutate(Country = "USA")

#identify & prep inst missing data----
no_carn_surv_inst <- anti_join(clean_surv_inst, all_matches, 
                     by = c("id", "inst_type")) %>% 
  mutate(inst_name = map(inst_name, fix_non_carn),
         inst_name = unlist(inst_name))

#source non-carn universities/institutions----
non_carn_unis <- read_csv("data/non-carnegie_unis.csv") %>% ## Need to work on matching inst names for non-carn inst
  mutate(Institution = str_to_title(Institution),
         Institution = str_remove_all(Institution, "'"),
         `Full Institution Name` = str_remove_all(`Full Institution Name`, "'"),
         `Full Institution Name` = str_to_title(`Full Institution Name`))

#merge non-carn data w/ inst missing data----
non_carn_join <- left_join(no_carn_surv_inst, non_carn_unis, 
                           by = c("inst_name" = "Institution")) %>% 
  left_join(., non_carn_unis, 
            by = c("inst_name" = "Full Institution Name")) %>% 
  mutate(State_Providence = if_else(!is.na(State_Providence.x), State_Providence.x, State_Providence.y),
         Country = if_else(!is.na(Country.x), Country.x, Country.y),
         PUI_RI = if_else(!is.na(PUI_RI.x), PUI_RI.x, PUI_RI.y),
         Other_inst_type = if_else(!is.na(Other_inst_type.x), Other_inst_type.x, Other_inst_type.y)) %>% 
  select(-contains(".x"), -contains(".y")) %>% 
  left_join(., us_regions, by = c("State_Providence" = "State_abbvr")) %>% 
  select(-State_name) %>% 
  rename(US_region = Region, NAME = inst_name) %>% 
  mutate(world_region = map(Country, get_world_region),
         world_region = unlist(world_region))

all_inst_data <- bind_rows(carn_region_join, non_carn_join) %>% 
  mutate(world_region = if_else(is.na(world_region), Country, world_region))

write_csv(all_inst_data, "data/full_survey_inst_data.csv")

