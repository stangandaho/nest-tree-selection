library(tidyverse)
library(showtext)

# Import data
survey_data <- read.csv(file = "data/survey_data.csv")

# Function to comput proportion
prop_fun <- function(x){x*100/sum(x)}

#🔥 Tree species proportion plot
plot_df <- survey_data %>% 
  group_by(scientific_name) %>% 
  summarise(num = n()) %>% 
  mutate(prop = round(prop_fun(num), 2),
         scientific_name = str_wrap(scientific_name, 5),
         scientific_name = case_when(scientific_name == "Diospyros\nmespiliformis" ~ "Diospyros mespiliformis",
                                     TRUE ~ scientific_name)) %>% 
  arrange(prop)
plot_df$scientific_name <- factor(plot_df$scientific_name, 
                                 levels = plot_df$scientific_name)

##
source("scripts/fonts_and_colors.R")

ggplot(data = plot_df)+
  geom_col((aes(x = scientific_name, y = prop)))+
  geom_col(data = data.frame(x = plot_df$scientific_name, y = plot_df$prop + effect_len), 
             aes(x = x, y = y, color = y), width = 0.01, show.legend = F)+
  geom_col((aes(x = scientific_name, y = prop, fill = prop)), show.legend = F)+
  geom_point(aes(x = scientific_name, y = prop + effect_len, color = prop), 
             size = 2, show.legend = F)+
  geom_text(data = data.frame(scientific_name = plot_df$scientific_name,
                              y = if_else(plot_df$scientific_name != "Diospyros mespiliformis", 
                                             plot_df$prop + 35, plot_df$prop - 23),
                              prop = plot_df$prop),
            mapping = aes(x = scientific_name, y = y, label = paste0(scientific_name, "\n(", prop,"%", ")")),
            lineheight = .3, angle = if_else(plot_df$scientific_name == "Diospyros mespiliformis", 280, 0),
            family = if_else(plot_df$scientific_name == "Diospyros mespiliformis", "msbi", "mmi"), size = 16)+
  ylim(-40, 50)+
  scale_fill_gradientn(colours = plt_color)+
  scale_color_gradientn(colours = plt_color)+
  theme_void()+
  theme(
    plot.margin = margin(0,0,0,0),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )+
  coord_polar()

# Save
ggsave(filename = "plots/tree_species_proportion.jpeg", 
       width = 30, height = 30, units = "cm", dpi = 300)


#🔥 Vulture Presence per tree species
vulpre_df <- survey_data %>% 
  select(vulture_presence, scientific_name) %>% 
  group_by(vulture_presence, scientific_name) %>% 
  summarise(num = n(), .groups = "drop") %>% 
  mutate(prop = round(num*100/sum(num), 2)) %>% 
  arrange(prop)

ggplot(data = vulpre_df, aes(x = str_wrap(scientific_name, 5), y = prop, group = vulture_presence))+
  geom_col(mapping = aes(fill = factor(vulture_presence, 
                                       levels = c("No nest", "Large nest", "Vulture nest"))),
           position = position_fill())+
  geom_text(aes(label = paste(round(prop, 2), "%"), y = prop), family = "msb",
            position = position_fill(vjust = .5), color = "white", size = 15, angle = 90)+
  labs(fill = "")+
  scale_y_continuous(expand = c(0.01, 0.001)) +
  scale_fill_manual(values = plt_color[c(1,3,4)])+
  scale_color_manual(values = plt_color[c(1,3,4)])+
  theme_void()+
  theme(
    plot.margin = margin(0,0,0,0),
    axis.title = element_blank(), axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, family = "mmi", size = 55, 
                             lineheight = 0.22),
    axis.ticks = element_blank(),
    legend.text = element_text(family = "mr", size = 55, angle = 90),
    legend.location = "plot", legend.text.position = "top", 
    legend.key.spacing.y = unit(2, "lines"), legend.box.margin = margin(r = 2, unit = "lines")
  )
ggsave(filename = "plots/vulture_presence_per_tree.jpeg", 
       width = 40, height = 30, units = "cm", dpi = 300)
# Proportion d'occurence des nids de White-backed Vulture d'autre nids larges sur differentes especes d'arbres sondes 