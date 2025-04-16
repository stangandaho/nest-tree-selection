survey_data <- read.csv(file = "data/survey_data.csv") %>% 
  mutate(Fire.Damage = if_else(is.na(Fire.Damage), 0, Fire.Damage),
         Inssect.Damage = if_else(is.na(Inssect.Damage), 0, Inssect.Damage),
         Debarking = if_else(is.na(Debarking), 0, Debarking))


survey_data %>% tidyr::drop_na(Fire.Damage) %>% 
  dplyr::count(Fire.Damage) %>% 
  dplyr::mutate(p = (n/sum(n))*100)

indam <- survey_data %>%# tidyr::drop_na(Inssect.Damage) %>% 
  dplyr::count(Inssect.Damage) %>% 
  dplyr::mutate(p = round((n/sum(n))*100, 1))

paste0("n = ", indam$n, ", ", indam$p, "%")
sum(indam$n)

dedam <- survey_data %>%# tidyr::drop_na(Inssect.Damage) %>% 
  dplyr::count(Debarking) %>% 
  dplyr::mutate(p = round((n/sum(n))*100, 1))
