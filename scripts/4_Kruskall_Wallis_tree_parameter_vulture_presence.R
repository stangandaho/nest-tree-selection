# Import data
library(dplyr)
library(rstatix)
library(showtext)

survey_data <- read.csv(file = "data/survey_data.csv") %>% 
  mutate(CBH1 = CBH1/100, CBH2 = CBH2/100) %>% 
  rename(`Canopy width` = Canopy.width, Height = Heigth)

# Test if vulture presence is associated to characteristics (Heigth, CBH 1, 2 and Canopy width)
tree_params <- survey_data[, c("vulture_presence", "Height", "CBH1", "CBH2", "Canopy width")] 

# Check normality
for (d in colnames(tree_params[, -1])) {
  piro <- shapiro.test(tree_params[[d]])
  if (piro$p.value > 0.05) {
    print(paste(d, "follows normal distribution 👍"))
  }else{
    print(paste(d, "doesn't follow normal distribution👎"))
  }
}

#-> Any variable follows normal distribution

# So perform non parametric test 
# Create a function to return p-value significant symbol
pval_sign <- function(x){
  if (x >= 0 & x < 0.001) {
    sgn <- "***"
  }else if(x >= 0.001 & x < 0.01){
    sgn <- "**"
  }else if (x >= 0.01 & x < 0.05) {
    sgn <- "*"
  }else{
    sgn <- "ns"
  }
  return(sgn)
}

# Create empty data frame to store each Kruskal–Wallis test output
kt_all <- data.frame()
for (p in colnames(tree_params[, -1])) {
  df4test <- tree_params[, c("vulture_presence", p)]
  kt <- kruskal.test(g = df4test[["vulture_presence"]], 
                     x = df4test[[p]])
  kt_df <- data.frame(variable = p, statistic = round(kt$statistic, 3), 
                      pvalue = kt$p.value, sign = pval_sign(kt$p.value))
  rownames(kt_df) <- NULL
  kt_all <- rbind(kt_all, kt_df)
}
write.csv(kt_all, "tables/kuskall_test.csv", row.names = F, fileEncoding = "ISO-8859-1")

# Do post hoc test for which Kruskal–Wallis test is significative
# Select only variable for which Kruskal–Wallis test is significant
sign_kt <- kt_all %>% dplyr::filter(sign != "ns") %>% pull(variable)

ph_df <- tree_params[, c("vulture_presence", sign_kt)]

# Create empty data frame to store each Dunn test output
duntes_all <- data.frame()
for (vr in names(ph_df[, -1])) {
  duntest <- rstatix::dunn_test(data = ph_df, 
                       formula = as.formula(paste0(vr, " ~ ", "vulture_presence")),
                       p.adjust.method = "bonferroni", detailed = T) %>% 
    mutate(max_m = max(ph_df[[vr]], na.rm = T)-0.5)
  duntes_all <- rbind(duntes_all, duntest)
  
}

## Plot
##
effect_len <- 20
plt_color <- plt_manu <- c( "#6C5B7B","#C06C84","#F67280","#F8B195")
font_add("mr", "fonts/montserrat/Montserrat-Regular.ttf")
font_add("mmi", "fonts/montserrat/Montserrat-MediumItalic.ttf")
font_add("msb", "fonts/montserrat/Montserrat-SemiBold.ttf")
font_add("msbi", "fonts/montserrat/Montserrat-SemiBoldItalic.ttf")
showtext_auto()

duntes_boxplot <- duntes_all %>% 
  rstatix::add_xy_position()

ggpubr::ggboxplot(tree_params, x = "vulture_presence", 
                  y = c("CBH1", "CBH2", "Height", "Canopy width"), 
                  color = "#F8B195", fill = "#6C5B7B", combine = T,
                   scale = "free_y")+
  ggpubr::stat_pvalue_manual(duntes_boxplot, hide.ns = TRUE, "y.position" = "max_m",
                             label = "Dunn test, p = {round(p,3)} {p.adj.signif}   Estimate = {round(estimate,2)}",
                             label.size = 13, family = "mr", lineheight = 0.0)+
  theme(
    plot.margin = margin(0,0,0,0), 
    panel.background = element_rect(color = "#6C5B7B", size = 3),
    panel.border = element_rect(color = NA),
    axis.title = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(hjust = 0.5, family = "mmi", size = 40, lineheight = 0.22),
    strip.background = element_rect(fill = plt_color[1], color = plt_color[1]),
    strip.text = element_text(size = 40, family = "msb", color = "#FFFFFF")
  )

# Save
ggsave(filename = "plots/dun_test_ploxplot.jpeg", 
       width = 25, height = 20, units = "cm", dpi = 300)


## 
duntes_all <- duntes_all %>% 
  dplyr::select(-all_of(c("estimate1", "estimate2", "method", "p.adj"))) 
write.csv(duntes_all, "tables/dunn_test_signif.csv", row.names = F, fileEncoding = "ISO-8859-1")
