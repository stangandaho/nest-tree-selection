library(readxl)
library(ggplot2)
library(showtext)
library(stringr)
library(dplyr)
library(tidyr)
library(ggimage)


# Plot customization
source("scripts/fonts_and_colors.R")
title_text_size <- 100
subtitle_text_size <- 80
axis_text_size <- 50
legend_text_size <- 40
caption_text_size <- 35


##
threatened <- read_excel(path = "data/threatened.xlsx") %>% 
  mutate(category = str_to_title(category))
## Add image path
image_icon <- paste0("./images/redlist/", threatened$category, ".svg")
threatened_df <- threatened %>% 
  mutate(image_icon = as.character(image_icon)) %>% 
  pivot_longer(cols = !c(category, image_icon), names_to = "status", values_to = "percentage") %>% 
  mutate(status = str_to_sentence(status),
         category = str_wrap(category, 10),
         # Add column to use to position image
         icon_position = 1)
  

ggplot(data = threatened_df, aes(x = category, y = percentage, group = status))+
  geom_col(aes(fill = status), position = position_fill(vjust = .5), width = 1, colour = "white")+
  ggimage::geom_image(aes(fill = status, y = icon_position, image =  image_icon), 
                      color = "#6C5B7B", 
                      size = ifelse(threatened_df$category == "Reef\nCorals" | threatened_df$category == "Conifers" , 0.05, 0.1))+
  scale_color_manual(values = plt_color[2:1])+
  scale_fill_manual(values = plt_color[2:1])+
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_fill(vjust = .5), 
            colour = "white", size = 18, family = "mb")+
  labs(fill = "", caption = "By Stanislas Mahussi Gandaho | Data source: IUCN Red List - iucnredlist.org",
       title = "Threatened Species with Extinction", 
       subtitle = "28% of all assessed species")+
  theme_void()+
  scale_y_continuous(expand = c(-.82, .88))+
  theme(
    plot.margin = margin(b = 1, t = .4, r = 1, unit = "lines"),
    axis.text.x = element_text(color = "#6C5B7B", size = axis_text_size, family = "msb",
                               lineheight = 0.25, margin = margin(b = 1, unit = "lines"),
                               vjust = 1, hjust = 0.5),
    legend.position = c(0.12, -.08), 
    legend.direction = "horizontal",
    legend.text = element_text(size = legend_text_size, color = "#6C5B7B", 
                               family = "msbi"),
    legend.margin = margin(t = 1, unit = "lines"),
    plot.caption = element_text(hjust = 1, size = caption_text_size, family = "mr", color = "gray40"),
    plot.title = element_text(size = title_text_size, color = "#F67280", family = "meb", hjust = 1),
    plot.subtitle = element_text(size = subtitle_text_size, color = "#6C5B7B", family = "msb", hjust = 1,
                                 margin = margin(b = 1, unit = "lines"))
  )
ggsave("plots/threatened_species.jpeg", width = 40, height = 25, units = "cm")
