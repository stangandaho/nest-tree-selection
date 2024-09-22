# Import data
library(dplyr)
library(ggplot2)

survey_data <- read.csv(file = "data/survey_data.csv")


#🔥 Test if tree health status is associated to characteristics (Heigth, CBH 1, 2 and Canopy width)
health_status <- survey_data[, c("health_level", "Heigth", "CBH1", "CBH2", "Canopy.width")] %>% 
  rename(`Canopy width` = Canopy.width) %>% 
  mutate(CBH1 = CBH1/100, CBH2 = CBH2/100)

# Check normality
for (d in colnames(health_status[, -1])) {
  piro <- shapiro.test(health_status[[d]])
  if (piro$p.value > 0.05) {
    print(paste(d, "follows normal distribution 👍"))
  }else{
    print(paste(d, "doesn't follow normal distribution👎"))
  }
  rm(piro, d)
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
for (p in colnames(health_status[, -1])) {
  df4test <- health_status[, c("health_level", p)]
  kt <- kruskal.test(g = df4test[["health_level"]], 
                     x = df4test[[p]])
  kt_df <- data.frame(variable = p, statistic = round(kt$statistic, 3), 
                      pvalue = kt$p.value, sign = pval_sign(kt$p.value))
  rownames(kt_df) <- NULL
  kt_all <- rbind(kt_all, kt_df)
  rm(kt, kt_df, df4test, p)
}
write.csv(kt_all, "tables/health_status_kuskall_test.csv", row.names = F, fileEncoding = "ISO-8859-1")

# RESULT 
# All significant are 'ns' (non significative). So tree health status is not associated to 
# tree characteristics (Heigth, CBH 1, 2 and Canopy width)

#🔥 Check if vulture presence depend to tree health
dep_tb <- survey_data %>% 
  select(vulture_presence, health_level) %>% 
  table() %>% 
  fisher.test()
# RESULT
# p-value = 0.3465 so there is no association between vulture and tree health