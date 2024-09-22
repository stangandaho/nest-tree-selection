# Import data
library(dplyr)
library(ggplot2)
library(showtext)

survey_data <- read.csv(file = "data/survey_data.csv")

## Plot
source("scripts/fonts_and_colors.R")

survey_data %>% 
  group_by(health_level) %>% 
  summarise(number = n()) %>% 
  mutate(prop = number*100/sum(number)) %>% 
  ggplot2::ggplot(aes(group = health_level))+
  geom_col(aes(x = health_level, y = prop), fill = plt_color[1], 
           position = position_stack())+
  geom_text(aes(x = health_level, y = prop-2, label = health_level, group = health_level), 
            color = "#FFFFFF", size = 24, family = "msb")+
  geom_text(aes(x = health_level, y = prop-5, label = paste0(round(prop,2), "%"), group = health_level), 
            color = plt_color[4], size = 30, family = "msb")+
  theme_void()

# Save
ggsave(filename = "plots/health_status_proportion.jpeg", 
       width = 25, height = 20, units = "cm", dpi = 300)

  
unhealthy_tree <- survey_data %>% 
  select(health_level, scientific_name) %>% 
  dplyr::filter(health_level == "Very unhealthy") %>% 
  group_by(scientific_name) %>% 
  summarise(number = n()) %>% 
  mutate(prop = number*100/sum(number),
         scientific_name = stringr::str_wrap(scientific_name,5)) %>% 
  arrange(prop)
unhealthy_tree$scientific_name <- factor(unhealthy_tree$scientific_name,
                                         levels = unique(unhealthy_tree$scientific_name))
unhealthy_tree %>% 
  ggplot2::ggplot(aes(group = scientific_name))+
  geom_col(aes(x = scientific_name, y = prop+1, fill = prop), show.legend = F)+
  scale_fill_gradientn(colours = plt_color)+
  geom_text(aes(x = scientific_name, y = (prop-prop)+0.1, label = scientific_name), 
            color = "#FFFFFF", size = 18, family = "msbi", lineheight = 0.3, 
            hjust = 0, angle = 90)+
  geom_text(aes(x = scientific_name, y = prop+1.5, label = paste0(round(prop,2), "%"),
                color = prop), size = 18, family = "msb", show.legend = F)+
  scale_color_gradientn(colors = rev(plt_color))+
  theme_void()

# Save
ggsave(filename = "plots/unhealthy_tree_proportion.jpeg", 
       width = 25, height = 20, units = "cm", dpi = 300)