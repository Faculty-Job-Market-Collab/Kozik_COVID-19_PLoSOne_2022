library(tidyverse)
library(lubridate)

# Load job add data from HERC

HERC_file_list <- list.files(path = "data/herc_job_data/", pattern = "PROJECT*", full.names = TRUE)

herc_data_list <- map_df(HERC_file_list, read_csv)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

clean_herc_data <- herc_data_list %>% #head(100) %>% 
  select(-EmployerDisplay, -Requirements, -DateTimeClosing) %>% 
  mutate(Description = str_to_lower(Description),
         Description = map(Description, function(x){str_replace_all(x, "<.*?>|-", " ")}),
         Description = unlist(Description),
         DateTimePosted = date(DateTimePosted),
         DateTimeClosed = date(DateTimeClosed),
         TenureTrack = case_when(
           str_detect(Description, "non tenure track") ~ "No",
           str_detect(Description, "tenure track") ~ "Yes",
           TRUE ~ "No"),
         NumPositions = str_extract(Description, "[:digit:]*(?= positions)"),
         YearPosted = year(DateTimePosted) %>% as_factor(.),
         MonthPosted = month(DateTimePosted, label = TRUE),
         MonthPosted = factor(MonthPosted, levels = month_levels))

write_csv(clean_herc_data, paste0("merged_herc_data_18-22_", Sys.Date(), ".csv"))