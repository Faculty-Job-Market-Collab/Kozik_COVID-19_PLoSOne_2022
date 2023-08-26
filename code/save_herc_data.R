library(tidyverse)
library(lubridate)

# Load job add data from HERC

HERC_file_list <- list.files(path = "data/herc_raw_files/", 
                             pattern = "*.csv", full.names = TRUE)

herc_data_list <- map_df(HERC_file_list, read_csv)

#month_levels <- c(
#  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
#  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
#)

non_us_list <- c("Soweto|Kathmandu|Kampala|Mbabane|Abu Dhabi|United Arab|Buenos Aires|China|Abidjan|Riyadh|Saudi Arabia|Weihai|Grenada|Johannesburg|Tashkent|Trinidad and Tobago|Kinshasa|Berlin$|Tonga|Yeonsugu|Switzerland|Luxembourg|Quetzaltenango|Prague|Pristina|India$|CERN|Oesterreich|Dhaka|Nairobi|Rwanda|Israel|Ningbo|Kigali|Prussia|Hong Kong|Auckland|Dryden$|Addis Ababa|Ethiopia|Geneva|Paris$|France|London UK|Spain|Africa|Kazakhstan|Germany|Nova Scotia|Canada|Tel Aviv|United Kingdom|Palestine|Shanghai|Outside USA|Nigeria|Doha|Qatar")

all_herc_data <- herc_data_list %>% #head(10000) %>% 
  select(-EmployerDisplay, -Requirements, -DateTimeClosing, -CategoryList, -DateTimeClosed) %>% 
  rowid_to_column(., "JobID") %>% 
  mutate(LocationDisplay = str_remove_all(LocationDisplay, "[[:digit:]]|[[:punct:]]"),
         Country = if_else(str_detect(LocationDisplay, non_us_list), "International", "USA")) %>% 
  mutate(Description = str_to_lower(Description),
         Description = map(Description, function(x){str_replace_all(x, "<.*?>|-", " ")}),
         Description = unlist(Description),
         DateTimePosted = date(DateTimePosted))

write_csv(all_herc_data, paste0("data/merged_herc_data_18-22_", Sys.Date(), ".csv"))

rm(HERC_file_list)
rm(herc_data_list)
gc()

clean_herc_data <- all_herc_data %>% #head(n=10000) %>% 
  mutate(TenureTrack = case_when(
           str_detect(Description, "non tenure track|nontenure") ~ "No",
           str_detect(Description, "tenure track") ~ "Yes",
           TRUE ~ "No"),
         NonTrack = if_else(str_detect(Description, "non tenure track|nontenure"), "Yes", "No"),
         Fixed_term = if_else(str_detect(Description, "fixed term"), "Yes", "No"),
         ECR = case_when(
           str_detect(Title, "Assistant(?=.*Prof)") ~ "Yes",
           str_detect(Description, "assistant teaching professor|assistant,|assistant professor") ~ "Yes",
           TRUE ~ "No"
           ),
         Adjunct = case_when(
           JobType == "Adjunct" ~ "Yes",
           str_detect(Title, "Adjunct") ~ "Yes",
           str_detect(Description, "adjunct") ~ "Yes",
           TRUE ~ "No"
         ),
         Postdoc = case_when(
           str_detect(Title, "Postdoc|POSTDOC") ~ "Yes",
           str_detect(Description, "postdoc(?!toral or equivalent)") ~ "Yes", 
           TRUE ~ "No"
           ),
         NumPosDesc = str_extract(Description, "[:digit:]*(?= positions)"),
         NumPositions = ifelse(!is.na(OpeningsQty), OpeningsQty,NumPosDesc),
         YearPosted = year(DateTimePosted) %>% as_factor(.),
         MonthPosted = lubridate::month(DateTimePosted, label = TRUE, abbr = TRUE))

write_csv(clean_herc_data, paste0("data/clean_herc_data_18-22_", Sys.Date(), ".csv"))

rm(all_herc_data)
gc()

final_herc_data <- clean_herc_data %>% 
  filter(Postdoc != "Yes") %>% 
  select(-DateTimePosted, -LocationDisplay, -NumPosDesc, -OpeningsQty,
         -JobType, -EmployerName, -Description, -Postdoc)

write_csv(final_herc_data, paste0("data/final_herc_data_18-22_", Sys.Date(), ".csv"))