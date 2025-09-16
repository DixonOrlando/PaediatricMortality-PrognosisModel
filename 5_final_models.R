###Building final models###

library(tidyverse)
library(haven)
library(naniar)
library(visdat)
library(tidyselect)
library(do)
library(broom)
library(eeptools)
library(gtsummary)
library(mice)
library(tidymodels)
library(rlang)
library(themis)
library(yardstick)
library(ggparallel)
library(mice)
library(vip)
library(probably)
library(xgboost)
library(MLmetrics)
library(CalibrationCurves)
library(meta)
library(pimeta)
library(rpart.plot)
library(rpart)
library(ggpubr)
library(DescTools)
library(pROC)
library(grid)
library(visreg)
library(regressinator)
library(mice)
library(miceadds)
library(tcltk)
library(glmm)
library(lme4)
library(pseudo)
library(survival)
library(this.path)

setwd(this.path::here())

###########READING THE DATA###########################

clean <- read_dta("CRF1childmaster_cleaned.dta")

clean <- clean %>%  # Create new column, length of stay 
  
  mutate(DODnew = ymd(DOD)) %>% # date of death or discharge 
  mutate(DOAnew = ymd(DOA)) %>%  
  mutate(AgeD = as.numeric(DODnew-DOAnew, na.rm=TRUE)) 

clean %>% #Checking the distribution of Died beyond 7 days.
  filter(AgeD >8) %>%
  count(Died)

clean2 <- clean %>% ###########CORRECT
  filter(StudyPeriod == 1 | StudyPeriod == 2) %>%  #Restrict to these time periods as data most reliable
  filter(Q13DOA2>=2016) %>% # High % missing SpO2 in POx only period in 2015
  drop_na(Died) %>% # N=54
  mutate(Died = as.factor(if_else(Died == 1 & AgeD <8, 1, 0))) %>% # Assigns late deaths to alive. Omit this row if want in hospital mortality as outcome
  
  mutate(SpO2cat = case_when( 
    is.na(Q52SP02P) | Q52SP02P <= 30 ~ "Missing",
    Q52SP02P >=90 ~ "More_than_equal_90",
    Q52SP02P < 80 ~ "Less_than_80",
    TRUE ~ ".80_to_89")) %>%
  mutate(SpO2cat= as.factor(SpO2cat)) %>%
  mutate(SpO2cat = relevel(SpO2cat, ref = "Missing"))  %>%
  mutate(Malnutrition = as.factor(case_when( # This adds cases from SAM to the in Malnutrition
    AdDxNUT==1 ~ "Yes",
    SAM == 1 ~ "Yes", 
    is.na(AdDxNUT) ~ NA_character_,
    TRUE ~ "No"))) %>%
  dplyr::select (SpO2cat, age2, Q12SEX, Q005HEAL, HospType, Q35FEVER2, Q36COUGH2, Q41DIARH2, Q37RESPI2, Q38CYANO2, Q39PALLO2, Q42UNBRE2, Q43CONVU2, Q44CONFU2, Q45COMA2, Q46SIGNO2, Q47DEHYD2, Q50HEART, Q51RESPI, Q52SP02P, Malnutrition, Died, AdDxLRTI, AgeGroup, Q006STUD,AdDxMAL, AdDxLRTI, AdDxDIAR, AdDxNUT, AdDxSEPTIC, AdDxURTI, AdDxASTH, AdDxTYPH, AdDxHIV, AdDxSEIZ, AdDxMENG, AdDxHAEM, AdDxSKIN, AdDxTRAUMA, AdDxNEPH, AdDxABDO, AdDxSEPS, AgeD, SAM, FullOxygen, Step)

clean_child <- clean2 %>%
  filter(AgeGroup != 1)


clean_child_final <- clean_child %>%
  drop_na (Q37RESPI2) %>% #Dropped this because there are multiple participants with missing values for many clinical sign variables.
  filter (age2 >0 & age2 < (12*13)) #Implementing <13 years cut-off


clean_child_who <- clean_child_final %>%  #Using the WHO threshold recommendation.
  
  mutate (HRcat = case_when( 
    
    is.na(Q50HEART) ~ NA_character_, 
    
    Q50HEART < 100 & age2 <12 ~ "Low", 
    
    Q50HEART < 80 & age2 %in% c(12:60) ~ "Low", 
    
    Q50HEART < 70 & age2 >59.9 ~ "Low", 
    
    Q50HEART > 160 & age2 <12 ~ "High", 
    
    Q50HEART > 150 & age2 %in% c(12:60) ~ "High", 
    
    Q50HEART > 130 & age2 >59.9 ~ "High", 
    
    TRUE ~ "Normal")) %>% 
  
  mutate(HRcat= as.factor(HRcat)) %>% 
  
  mutate (HRcat = relevel(HRcat, ref = "Normal")) %>% 
  
  mutate (RRcat = case_when( 
    
    is.na(Q51RESPI) ~ NA_character_, 
    
    Q51RESPI < 25 & age2 <12 ~ "Low", 
    
    Q51RESPI < 20 & age2 %in% c(12:60) ~ "Low", 
    
    Q51RESPI < 15 & age2 >59.9 ~ "Low", 
    
    Q51RESPI > 50 & age2 <12 ~ "High", 
    
    Q51RESPI > 40 & age2 %in% c(12:60) ~ "High", 
    
    Q51RESPI > 30 & age2 >59.9 ~ "High", 
    
    TRUE ~ "Normal")) %>% 
  
  mutate(RRcat= as.factor(RRcat)) %>% 
  
  mutate (RRcat = relevel(RRcat, ref = "Normal")) %>% 
  
  mutate (agecat = case_when( 
    
    age2 <12 ~ "Age<1", 
    
    age2 >= 12 & age2< 24 ~ "1<=Age<2", 
    
    age2 >= 24 & age2 < 60 ~ "2<=Age<5", 
    
    # age2 >=60 & age2 < 120 ~ "under10", 
    
    # age2 >=120 ~ "under17" 
    
    age2 >=60 ~ "5<=Age<13")) %>% 
  mutate (agecat = factor(agecat, levels = c("Age<1", "1<=Age<2", "2<=Age<5", "5<=Age<13"))) 


distinct_func <- function(x) { #Converting double variable into factor variable if they have more than two levels.
  return(n_distinct(x, na.rm=T))
}

values_count <- lapply(clean_child_who, distinct_func)

clean_child_who[,values_count <= 12] <- lapply(clean_child_who[,values_count <= 12] , factor) #Converting the double into factor for easier handling later.

clean_child_who <- clean_child_who %>% 
  dplyr::mutate(SpO2cat = factor(SpO2cat, levels = c("More_than_equal_90", ".80_to_89", "Less_than_80", "Missing")),
                HRcat = factor(HRcat, levels = c("Normal","Low", "High")),
                RRcat = factor(RRcat, levels = c("Normal", "Low", "High")),
                AdDxLRTI = if_else(AdDxLRTI == "2", "1", AdDxLRTI)) 


clean_child_who_mutate <- clean_child_who %>%
  mutate(Q52SP02P = if_else(Q52SP02P <=30, NA, Q52SP02P),
         Q52SP02P = as.numeric(Q52SP02P)) %>%
  dplyr::select(-Q006STUD, -AgeGroup)

clean_child_who_mutate_drop <- clean_child_who_mutate %>%
  drop_na(AdDxLRTI, AdDxMAL, AdDxDIAR, AdDxNUT, AdDxSEPTIC, AdDxURTI, AdDxASTH, AdDxTYPH, AdDxHIV, AdDxSEIZ, AdDxMENG, AdDxHAEM, AdDxSKIN, AdDxTRAUMA, AdDxNEPH, AdDxABDO, AdDxSEPS) %>% 
  dplyr::select(-AdDxNUT, -AdDxSEPS, -AdDxNEPH) #Dropping observations with many missingness across all candidate predictors


clean_child_who_mutate_drop <- clean_child_who_mutate_drop %>%
  mutate(cluster_code = Q005HEAL)

clean_child_who_mutate_drop <- clean_child_who_mutate_drop %>%
  mutate(cluster_code = as.factor(cluster_code),
         AdDxLRTI = as.factor(AdDxLRTI))



final_child <- clean_child_who_mutate_drop

final_child = final_child %>%
  drop_na(AgeD)

###########Final lasso logistic regression prior simplification###########

final_data = final_child %>%
  mutate(Died = factor(Died, levels = c("1","0")))

child_rec <- recipe(Died ~ Q12SEX + Q35FEVER2 + Q36COUGH2 + Q37RESPI2 + Q38CYANO2 + Q39PALLO2 + Q42UNBRE2 + Q43CONVU2 + Q44CONFU2 + Q45COMA2 + Q46SIGNO2 + Q47DEHYD2 + Malnutrition + AdDxLRTI + AdDxMAL + AdDxDIAR + AdDxSEPTIC + AdDxURTI + AdDxASTH + AdDxTYPH + AdDxHIV + AdDxSEIZ + AdDxMENG + AdDxHAEM + AdDxSKIN + AdDxTRAUMA + AdDxABDO + HRcat + RRcat + age2 + Q52SP02P + FullOxygen, 
                    data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_poly(all_numeric_predictors(), degree = tune()) %>%
  step_center(all_numeric_predictors(), -all_outcomes()) %>%
  step_scale(all_numeric_predictors(), -all_outcomes(), factor = 2) %>%
  step_dummy(all_nominal(), -all_outcomes()) 

wf <- workflow() %>%
  add_recipe(child_rec)

set.seed(1111)
child_boot <- vfold_cv(final_data, strata = Died, v = 5)

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet", standardize = FALSE) 

set.seed(1111)
lambda_grid <- grid_random(penalty(), degree = degree_int(range = c(2, 4)), 
                           size = 50)

set.seed(1111)
lasso_rs <- tune_grid(
  wf %>%
    add_model(tune_spec),
  resamples = child_boot,
  grid = lambda_grid,
  metrics = metric_set(mn_log_loss))

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), select_best(lasso_rs, metric = "mn_log_loss"))

lowest_logloss = select_best(lasso_rs, metric = "mn_log_loss")

set.seed(1111)

lasso_fit <- fit(final_lasso, data = final_data)

#Getting the variable importance
set.seed(1111)
lasso_varimp = lasso_fit %>%
  fit(final_data) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_logloss$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

###########Final lasso logistic regression after simplification###########

final_data = final_child %>%
  mutate(Died = factor(Died, levels = c("1","0")))

child_rec <- recipe(Died ~ Q45COMA2 + Malnutrition + Q52SP02P + Q37RESPI2 + AdDxMAL + HRcat, data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_poly(all_numeric_predictors(), degree = tune()) %>%
  step_center(all_numeric_predictors(), -all_outcomes()) %>%
  step_scale(all_numeric_predictors(), -all_outcomes(), factor = 2) %>%
  step_dummy(all_nominal(), -all_outcomes()) 

wf <- workflow() %>%
  add_recipe(child_rec)

set.seed(1111)
child_boot <- vfold_cv(final_data, strata = Died, v = 5)

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet", standardize = FALSE) 

set.seed(1111)
lambda_grid <- grid_random(penalty(), degree = degree_int(range = c(2, 4)), 
                           size = 50)

set.seed(1111)
lasso_rs <- tune_grid(
  wf %>%
    add_model(tune_spec),
  resamples = child_boot,
  grid = lambda_grid,
  metrics = metric_set(mn_log_loss))

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), select_best(lasso_rs, metric = "mn_log_loss"))

lowest_logloss = select_best(lasso_rs, metric = "mn_log_loss")

set.seed(1111)

lasso_fit <- fit(final_lasso, data = final_data)

#Getting the variable importance
set.seed(1111)
lasso_varimp = lasso_fit %>%
  fit(final_data) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_logloss$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

###########Final XGBoost prior simplification###########

final_data = final_child %>%
  mutate(Died = factor(Died, levels = c("1","0")))

set.seed(1111)
child_res_xg <- vfold_cv(final_data, 
                         v = 5,
                         strata = Died)

xgb_rec <- recipe(Died ~ Q12SEX + Q35FEVER2 + Q36COUGH2 + Q37RESPI2 + Q38CYANO2 + Q39PALLO2 + Q42UNBRE2 + Q43CONVU2 + Q44CONFU2 + Q45COMA2 + Q46SIGNO2 + Q47DEHYD2 + Malnutrition + AdDxLRTI + AdDxMAL + AdDxDIAR + AdDxSEPTIC + AdDxURTI + AdDxASTH + AdDxTYPH + AdDxHIV + AdDxSEIZ + AdDxMENG + AdDxHAEM + AdDxSKIN + AdDxTRAUMA + AdDxABDO + HRcat + RRcat + age2 + Q52SP02P + FullOxygen,
                  data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 


###RECIPE

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

set.seed(1111)
xgb_grid <- grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), final_data),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

set.seed(1111)

xgb_rs <- tune_grid(
  xgb_wf,
  resamples = child_res_xg,
  grid = xgb_grid,
  metrics = metric_set(mn_log_loss))

final_xgb <- finalize_workflow(xgb_wf, select_best(xgb_rs, metric = "mn_log_loss"))

xgb_fit <- fit(final_xgb, data = final_data)

#Getting the variable importance
set.seed(1111)
var_imp_xgb = xgb_fit %>%
  fit(data = final_data) %>%
  pull_workflow_fit() %>%
  vip(num_features = 32) 


###########Final XGBoost after simplification###########

final_data = final_child %>%
  mutate(Died = factor(Died, levels = c("1","0")))

set.seed(1111)
child_res_xg <- vfold_cv(final_data, 
                         v = 5,
                         strata = Died)

xgb_rec <- recipe(Died ~ Q52SP02P + Q45COMA2 + age2 + Q37RESPI2 + Q39PALLO2 + Malnutrition, data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 


###RECIPE

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

set.seed(1111)
xgb_grid <- grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), final_data),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

set.seed(1111)

xgb_rs <- tune_grid(
  xgb_wf,
  resamples = child_res_xg,
  grid = xgb_grid,
  metrics = metric_set(mn_log_loss))

final_xgb <- finalize_workflow(xgb_wf, select_best(xgb_rs, metric = "mn_log_loss"))

xgb_fit <- fit(final_xgb, data = final_data)

#Getting the variable importance
set.seed(1111)
var_imp_xgb = xgb_fit %>%
  fit(data = final_data) %>%
  pull_workflow_fit() %>%
  vip(num_features = 32) 


###########Final decision tree###########
final_data = final_child %>%
  mutate(Died = factor(Died, levels = c("1","0")),
         Q52SP02P = ifelse(is.na(Q52SP02P), 150, Q52SP02P))

set.seed(1111)
tree_folds <- vfold_cv(final_data, 
                       v = 5,
                       strata = Died)

tree_rec <- recipe(Died ~ Q12SEX + Q35FEVER2 + Q36COUGH2 + Q37RESPI2 + Q38CYANO2 + Q39PALLO2 + Q42UNBRE2 + Q43CONVU2 + Q44CONFU2 + Q45COMA2 + Q46SIGNO2 + Q47DEHYD2 + Malnutrition + AdDxLRTI + AdDxMAL + AdDxDIAR + AdDxSEPTIC + AdDxURTI + AdDxASTH + AdDxTYPH + AdDxHIV + AdDxSEIZ + AdDxMENG + AdDxHAEM + AdDxSKIN + AdDxTRAUMA + AdDxABDO + HRcat + RRcat + age2 + Q52SP02P + FullOxygen, data = final_data)

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

set.seed(1111)
tree_grid <- grid_random(
  cost_complexity(),
  tree_depth(c(1,5)),
  min_n(),
  size = 50
)

tree_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tree_spec)


set.seed(1111)
tree_rs <- tune_grid(
  tree_wf,
  resamples = tree_folds,
  grid = tree_grid,
  metrics = metric_set(mn_log_loss))

final_tree <- finalize_workflow(tree_wf, select_best(tree_rs, metric = "mn_log_loss"))

tree_fit = fit(final_tree, data =  final_data)

#Getting the variable importance
var_imp_DT = tree_fit %>%
  vip(geom = "col", num_features = 32) +
  scale_y_continuous(expand = c(0, 0)) 

#Getting the decision tree plot
rpart_plot = tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(extra = 104)


