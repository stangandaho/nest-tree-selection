library(tidyverse)
library(readxl)
source("scripts/trees_scientific_name.R")

# Import each sheet and bind them
##➡️ First part 
dpart1_sheet <- readxl::excel_sheets("data/Manyeleti_ERAIFT_Tree_Data.xlsx")

data_part1 <- tibble()
for (sh in dpart1_sheet) {
  df <- read_xlsx("data/Manyeleti_ERAIFT_Tree_Data.xlsx", sheet = sh) %>% 
    mutate(across(.cols = ends_with("Fungus"), .fns = as.character),
           across(.cols = ends_with("Notes"), .fns = as.character),
           across(.cols = ends_with("Latitude"), .fns = as.numeric),
           across(.cols = ends_with("Longitude"), .fns = as.numeric))
  data_part1 <- bind_rows(data_part1, df)
  rm(df)
}
## remove two last column
col_size <- length(colnames(data_part1))
data_part1 <- data_part1 %>% 
  select(- all_of((col_size - 1):col_size))

##➡️ Second part 
dpart2_sheet <- readxl::excel_sheets("data/Kempiana_ERAIFT_Tree_Data.xlsx")
data_part2 <- tibble()
for (sh in dpart2_sheet) {
  df <- read_xlsx("data/Kempiana_ERAIFT_Tree_Data.xlsx", sheet = sh) %>% 
    mutate(across(.cols = ends_with("Damage"), .fns = as.character),
           across(.cols = ends_with("Fungus"), .fns = as.character)) %>% 
    select(-Unknow)
  data_part2 <- bind_rows(data_part2, df)
  rm(df)
}

# Uniform column of two part and merge them
correct_colname <- colnames(data_part2)
colnames(data_part1) <- correct_colname

survey_data <- bind_rows(data_part1, data_part2) %>% 
  filter(`Tree no` != "2024-K129")

# Create health column
# A function to change the character vector to numeric vector
char2num <- function(x, chars = c(), nums = c()){
  x_new <- list()
  for (i in 1:length(x)) {
    if (any(x[i] %in% chars)) {
      indx_i <- which(chars == x[i])
      match_num <- nums[indx_i]
      x_new[[i]] <- match_num
    }else{
      x_new[[i]] <- x[i]
    }
  }
  names(x_new) <- NULL
  return(as.numeric(unlist(x_new)))
}

survey_data <- survey_data %>% 
  dplyr::mutate(
    across(Debarking:Fungus, ~ char2num(.x, chars = c("0.0", "0", "1.0", "L", "M", "H"), 
                                        nums = c(0, 0, 1, 1, 2, 3)))) %>%
  # Add health_level column
  rowwise() %>% mutate(health_level = sum(across(Debarking:Fungus), na.rm = T),
                       health_level = case_when(health_level <= 1 ~ "Healthy",
                                                health_level > 1 & health_level <= 3 ~ "Unhealthy",
                                                T ~ "Very unhealthy"),
                       # Tree scientific name column
                       scientific_name = tree_sn(`Tree species`)) %>% 
  mutate(vulture_presence = case_when(`Vulture presence` == 0 ~ "No nest",
                                      `Vulture presence` == 1 ~ "Large nest",
                                      `Vulture presence` == 2 ~ "Vulture nest")) %>% 
  select(-`Vulture presence`) %>% 
  relocate(`Tree no`, vulture_presence, scientific_name, health_level)

write.csv(x = survey_data, file = "data/survey_data.csv", row.names = F)
