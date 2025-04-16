# Import data
source("scripts/utils.R")
library(dplyr)
library(rstatix)
library(showtext)

survey_data <- read.csv(file = "data/survey_data.csv", check.names = FALSE) %>% 
  mutate(CBH1 = CBH1/100, CBH2 = CBH2/100,
         vulture_presence = case_when(vulture_presence == "No nest" ~ 0, TRUE ~ 1)) %>% 
  rename(Height = Heigth, `Insect Damage` = `Inssect Damage`) %>% 
  select(vulture_presence, CBH1, CBH2, Height, `Canopy width`, health_level) %>% 
  tidyr::drop_na()

## GLM 
damage <- survey_data %>% 
  dplyr::bind_rows(survey_data %>% dplyr::filter(vulture_presence == 1)) 

glm_mdl_damage <- glm(vulture_presence ~ ., data = survey_data, 
                      family = "binomial")
#summary(glm_mdl_damage)
stp <- step(glm_mdl_damage, direction = "both")
glm_mdl_damage <- summary(glm(stp$call$formula, data = damage, 
                              family = "binomial"))

# Built AIC table


mdl_list <- list(); mdl_names <- list()
for (i in 1:4) {
  acom <- combn(c("CBH1", "CBH2", "Height", "`Canopy width`", "health_level"),
                m = i)
  for (pred in 1:ncol(acom)) {
    formula <- paste0("vulture_presence ~ ", paste0(acom[, pred], collapse = " + "))
    formula <- as.formula(formula)
    mdl <- glm(formula = formula, data = survey_data, family = "binomial")
    mdl_list[[paste0(i,pred)]] <- mdl
    # Names
    mdl_name <- gsub("vulture_presence ~ ", "", as.character(mdl$formula)[3])
    mdl_names[[paste0(i,pred)]] <- mdl_name
  }
}

predictor_tab <- tibble(Modnames = names(mdl_names), 
       Predictors = unlist(mdl_names)) %>% 
  dplyr::mutate(Predictors = case_when(grepl("`", Predictors) ~ gsub("`", "", Predictors),
                              grepl("health_level", Predictors) ~ gsub("health_level", "Health level", Predictors),
                              TRUE ~ Predictors))

all_aictab <- AICcmodavg::aictab(cand.set = mdl_list) %>% 
  dplyr::tibble() %>% 
  dplyr::mutate(across(.cols = AICc:LL, .fns = \(x)round(x, digits = 2))) %>% 
  dplyr::left_join(y = predictor_tab, by = "Modnames") %>% 
  dplyr::select(-Modnames, -Cum.Wt) %>% 
  dplyr::relocate(Predictors, .before = 1) %>% 
  dplyr::rename(`Number of parameters` = K, `Delta AICc` = Delta_AICc,
                `Relative likelihood` = ModelLik,
                `Akaike weights` = AICcWt,
                `Log-likelihood` = LL)
  
write.csv(all_aictab, "tables/glm_aic.csv", row.names = FALSE)
######
bes_model <- glm(vulture_presence ~ CBH2, data = survey_data, 
                 family = "binomial")
selected_mdl <- glm_mdl_damage$coefficients %>% as.data.frame() %>% slice(-1)
rn <- rownames(selected_mdl)
selected_mdl <- selected_mdl %>% 
  mutate(predictor = rn) %>% 
  rowwise() %>% 
  mutate(sign = pval_sign(`Pr(>|z|)`))



## GLM tree size measure
