########################External validation########################

#Reading and cleaning the data.
kenya_data = read_csv("cin_data_27Jul_2025.csv")

#Converting variables into factor.

kenya_data = kenya_data %>%
  mutate_if(is.character, as.factor)

pre_final_kenya_data = kenya_data %>%
  mutate(DODnew = ymd(date_discharge)) %>% # date of death or discharge 
  mutate(DOAnew = ymd(date_adm)) %>%  
  mutate(AgeD = as.numeric(DODnew-DOAnew, na.rm=TRUE)) %>%
  drop_na(AgeD)

#Converting the -1 into NA, since -1 indicate missing value according to the codebook.
#14,058 missing values now for the age_years variable. 
pre_final_kenya_data = pre_final_kenya_data %>%
  mutate(age_years = ifelse(age_years == -1, NA, age_years),
         age_mths = if_else(age_mths < 0, NA, age_mths))

#Note about the age variables:
#1. If one is missing but not the other use the one that is not missing. The next conditions should be performed if only the first one is satisfied.
#2. If age_years is an integer and age_mths <12 and age_mths is an integer, then combine them.
#3. If age_years is not an integer then calculate if age_years*12 == age_mths. If satisfied use age_years, if not satisfied use age_mths.
#4. If age_mths is not an integer then calculate if age_years*12 == age_mths. If satisfied use age_years, , if not satisfied use age_mths.
#5. If age_mths >=12 then calculate if age_years *12 == age_mths. If satisfied use age_years, , if not satisfied use age_mths.
#6. If both are missing allocate NA and drop the observations.

#The age_final variable is in months.
pre_final_kenya_data = pre_final_kenya_data %>%
  mutate(age_final = case_when(
    #If one is missing, use the other one. 
    #Condition 1
    is.na(age_years) & !is.na(age_mths) ~ age_mths,
    #Condition 2
    !is.na(age_years) & is.na(age_mths) ~ age_years *12,
    #Condition 3.
    !is.na(age_years) & !is.na(age_mths) & age_years %% 1 == 0 & age_mths < 12 & age_mths %% 1 == 0  ~ age_years*12 + age_mths,
    #Condition 4
    !is.na(age_years) & !is.na(age_mths) & age_years %% 1 != 0 & round(age_years*12, 0) == age_mths ~ age_years*12,
    #Condition 4, but if not satisfied
    !(!is.na(age_years) & !is.na(age_mths) & age_years %% 1 != 0 & round(age_years*12, 0) == age_mths) ~ age_mths,
    #Condition 5
    !is.na(age_years) & !is.na(age_mths) & age_mths >=12 & round(age_years*12, 0) == age_mths ~ age_years*12,
    #Condition 5 if not satisfied
    !(!is.na(age_years) & !is.na(age_mths) & age_mths >=12 & round(age_years*12, 0) == age_mths) ~ age_mths,
    #Condition 6
    is.na(age_years) & is.na(age_mths) ~ NA
  ))


#Drop the observation with age_final equal to 1013.5. 
pre_final_kenya_data = pre_final_kenya_data %>%
  filter(age_final != max(pre_final_kenya_data$age_final, na.rm = T) | is.na(age_final))

#Filter observations so it only contain those below 13 years old. 
pre_final_kenya_data = pre_final_kenya_data %>%
  filter(age_final < 156 | is.na(age_final))

#Ensuring those below 1 month got excluded
pre_final_kenya_data = pre_final_kenya_data %>%
  filter(age_final >=1)

#Drop the observations with missing age variables.
pre_final_kenya_data = pre_final_kenya_data %>%
  drop_na(age_final)


#Censor the data after 7 days.
pre_final_kenya_data = pre_final_kenya_data %>% 
  mutate(outcome = if_else((AgeD >7 & outcome == "Died"), "Alive", outcome)) 

pre_final_kenya_data = pre_final_kenya_data %>%
  filter(outcome != "Empty" & !is.na(outcome)) %>% #Removing those with either NA or EMPTY outcome.
  filter(!(is.na(outcome_res) & outcome == "Alive")) %>% 
  filter(!(outcome_res == "Empty" & outcome == "Alive"))

#Deleting observations with negative AgeD value because we were not sure whether the date was swapped or if there was any error unknown to us.
pre_final_kenya_data = pre_final_kenya_data %>% 
  filter(AgeD >=0)



#continuous variables: oxygen_sat, blantyre_score, age_years, muac, resp_rate, pulse_rate

final_kenya_data = pre_final_kenya_data %>%
  mutate(oxygen_sat = ifelse(oxygen_sat <= 30 | oxygen_sat > 100, NA, oxygen_sat), #If lower than 30 then probably error; observation with negative value for Sp02 regarded as NA; oxygen_sat maximum value is 100.
         blantyre_score = ifelse(blantyre_score < 0 | blantyre_score >5, NA, blantyre_score),#Minimum value for blanty score is 0 and maximum is 5.
         muac = ifelse(muac < 0 | muac > 25, NA, muac), #MUAC can't be negative and the maximum value is 25.
         muac_mm = muac * 10, #The muac in the codebook is in CM, so multiply by 10 to convert it into mm (Hamish used mm)
         resp_rate = ifelse(resp_rate < 0 | resp_rate > 140, NA, resp_rate), #Less than zero to exclude negative and -1 (empty in codebook); >140 because following Shiraz advice, in which that higher than >=120 probably an error when recording the value
         pulse_rate = ifelse(pulse_rate < 20 | pulse_rate > 300, NA, pulse_rate)) %>% #Less than zero to exclude negative values; <20 (this automatically exclude negatives) and >300 because following Shiraz advice 
  mutate(across(where(is.factor), ~na_if(as.character(.x), "Empty") %>% factor())) #Changing the value "Empty" inside factors variable into NA.

#Converting all the missing values for factor to be new level called "Missing"
#Converting all the missing values for continuous variables to be 9999
#This is done so it's easier for data cleaning; NA can be tricky to write into code that uses a lot of conditional (if function).
final_kenya_data <- final_kenya_data %>%
  mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
  mutate(across(where(is.factor), ~fct_explicit_na(., na_level = "Missing"))) %>%
  mutate(across(where(is.numeric), ~replace_na(., 9999)))

############Starting to convert the variables############

#Q52SP02P
final_kenya_data = final_kenya_data %>%
  mutate(Q52SP02P = oxygen_sat)

#Excluding hospitals with high Sp02 missingness.
exclude_hospitals = final_kenya_data %>%
  count(oxygen_sat, hosp_id) %>%
  filter(oxygen_sat == 9999) %>%
  left_join(final_kenya_data %>%
              count(hosp_id) %>%
              rename('total' = 'n')) %>%
  mutate(prop = n/total * 100) %>%
  filter(prop >= 50) 


#Kisii (80), Kitale (51), Malindi (90.6), Migori (85.3), Thika (55.4), and World Friends (63.6).


final_kenya_data = final_kenya_data %>%
  filter(!(hosp_id %in% exclude_hospitals$hosp_id))

#exclude Mama Margaret Uhuru Hospital (n died = 6)

final_kenya_data = final_kenya_data %>%
  filter(hosp_id != "Mama Margaret Uhuru Hospital")

#Q45COMA2
final_kenya_data = final_kenya_data %>%
  mutate(avpu = as.character(avpu)) %>% #Converting into character first so when mutate it later it won't turn into numeric.
  mutate(avpu = ifelse(avpu == "Other scale", "Missing", avpu)) %>%
  mutate(
    Q45COMA2 = case_when(
      # 1. If Blantyre score is available
      blantyre_score != 9999 & blantyre_score <= 2 ~ 1,
      blantyre_score != 9999 & blantyre_score > 2 ~ 0,
      
      # 2. If Blantyre score missing, use AVPU
      blantyre_score == 9999 & avpu %in% c("Pain response", "Unresponsive") ~ 1,
      blantyre_score == 9999 & avpu %in% c("Alert", "Verbal response") ~ 0,
      
      # 3. If Blantyre and AVPU missing, use posture
      blantyre_score == 9999 & avpu == "Missing" & posture != "Missing" & posture == "Normal" ~ 0,
      blantyre_score == 9999 & avpu == "Missing" & posture != "Missing" & posture != "Normal" ~ 1,
      
      # Else still missing
      TRUE ~ 9999
    )
  ) 

#age2
final_kenya_data = final_kenya_data %>%
  mutate(age2 = age_final)

#Q37RESPI2 using grunting, acidotic_breathing, stridor

final_kenya_data = final_kenya_data %>%
  mutate(Q37RESPI2 = case_when(
    grunting == "Yes" | acidotic_breathing == "Yes" | stridor == "Yes" | (age2>60 & indrawing == "Yes") ~ 1,
    grunting == "No" & acidotic_breathing == "No" & stridor == "No" & !(age2>60 & indrawing == "Yes") ~ 0,
    TRUE ~ 9999
  )) 


#Q39PALLO2
final_kenya_data = final_kenya_data %>%
  mutate(Q39PALLO2 = case_when(
    pallor == "+ (mild/moderate)" | pallor == "+ + + (severe)" ~ 1,
    pallor == "none" ~ 0,
    pallor == "Missing" ~ 9999
  ))


#Malnutrition
final_kenya_data = final_kenya_data %>% #2nd condition where only considers when dx1_malnutr is equal to missing or mild and moderate.
  mutate(Malnutrition = case_when(
    !(dx1_malnutr %in% c("5B71,Mild", "5B71,Moderate", "E44.0-moderate", "E44.1-Mild")) & !(dx1_malnutr %in% c("Missing", "5C70.Z,Non Classified", "E86.9-No Classification")) ~ 1,
    dx1_malnutr %in% c("Missing", "5C70.Z,Non Classified", "E86.9-No Classification", "5B71,Mild", "5B71,Moderate", "E44.0-moderate", "E44.1-Mild") & (whz == "<=4SD" | muac_mm < 115| oedema %in% c("Face", "Foot", "Knee")) ~ 1,
    TRUE ~ 0
  ))

#Q47DEHYD2

final_kenya_data = final_kenya_data %>%
  mutate(Q47DEHYD2 = case_when(
    dx1_dehydrat %in% c("5C70&XS25,Severe dehydration", "E86.8-Severe", "MG40.1,Shock", "R57.1-Shock") & dx1_dehydrat != "Missing" & dx1_dehydrat != "5C70.Z,Non Classified" & dx1_dehydrat != "E86.9-No Classification" ~ 1,
    !(dx1_dehydrat %in% c("5C70&XS25,Severe dehydration", "E86.8-Severe", "MG40.1,Shock", "R57.1-Shock")) & dx1_dehydrat != "Missing" & dx1_dehydrat != "5C70.Z,Non Classified" & dx1_dehydrat != "E86.9-No Classification" ~ 0,
    TRUE ~ 9999
  ))

#RRcat variable

final_kenya_data = final_kenya_data %>%
  mutate(RRcat = case_when(
    age2==9999 | resp_rate == 9999 ~ "Missing",
    resp_rate < 25 & age2 <12 ~ "Low", 
    
    resp_rate < 20 & age2 %in% c(12:60) ~ "Low", 
    
    resp_rate < 15 & age2 >59.9 ~ "Low", 
    
    resp_rate > 50 & age2 <12 ~ "High", 
    
    resp_rate > 40 & age2 %in% c(12:60) ~ "High", 
    
    resp_rate > 30 & age2 >59.9 ~ "High", 
    
    TRUE ~ "Normal"
  ))


#HRcat variable

final_kenya_data = final_kenya_data %>%
  mutate(HRcat = case_when(
    age2==9999 | pulse_rate == 9999 ~ "Missing",
    pulse_rate < 100 & age2 <12 ~ "Low", 
    
    pulse_rate < 80 & age2 %in% c(12:60) ~ "Low", 
    
    pulse_rate < 70 & age2 >59.9 ~ "Low", 
    
    pulse_rate > 160 & age2 <12 ~ "High", 
    
    pulse_rate > 150 & age2 %in% c(12:60) ~ "High", 
    
    pulse_rate > 130 & age2 >59.9 ~ "High", 
    
    TRUE ~ "Normal"
  ))

#AdDxMAL

final_kenya_data = final_kenya_data %>%
  mutate(AdDxMAL = case_when(
    dx1_malaria == "Non Severe" ~ 1,
    dx1_malaria == "Severe" ~ 2,
    dx1_malaria == "Missing" ~ 0,
    dx1_malaria == "Non Classified" ~ 9999
  )) 

final_kenya_data_exclude = final_kenya_data %>%
  filter(!(AgeD <=7 & outcome == "Alive" & (outcome_res != "Discharged" | outcome_res == "Missing"))) 


#Converting the data into final form, with the appropriate format.

final_kenya = final_kenya_data %>%
  mutate_all(~ replace(., . == 9999, NA)) %>%
  mutate(Q45COMA2 = as.factor(as.character(Q45COMA2)),
         Q37RESPI2 = as.factor(as.character(Q37RESPI2)),
         Q47DEHYD2 = as.factor(as.character(Q47DEHYD2)),
         Malnutrition = factor(ifelse(Malnutrition == 1, "Yes", "No"), levels = c("No", "Yes"))) %>%
  mutate(Q39PALLO2 = as.factor(as.character(Q39PALLO2))) %>%
  mutate(RRcat = if_else(RRcat == "Missing", NA, RRcat),
         RRcat = factor(RRcat, levels = c("Normal", "Low", "High")),
         HRcat = if_else(HRcat == "Missing", NA, HRcat),
         HRcat = factor(HRcat, levels = c("Normal", "Low", "High")) ,
         outcome = if_else(outcome == "Missing", NA, outcome),
         outcome = factor(outcome, levels = c("Alive", "Died"))) %>%
  mutate(age2 = ifelse(age2 == 119988, NA, age2)) %>%
  mutate(Died = if_else(outcome == "Died", "1", "0"),
         Died = factor(Died, levels = c("0", "1"))) %>%
  mutate(AdDxMAL = factor(as.character(AdDxMAL), levels = c("0", "1", "2"))) 

#De-identify facilities
final_kenya = final_kenya %>%
  mutate(hosp_id = factor(as.numeric(hosp_id)))


######Perfoming case-mix check######

final_child = read_csv("final_child.csv") #Getting the Nigerian dataset.

#Building membership model

comb_data = simpen %>%
  dplyr::select(Q52SP02P, Q45COMA2, age2, Q37RESPI2, Q39PALLO2, Malnutrition, Q47DEHYD2, RRcat, HRcat, AdDxMAL,  Died) %>%
  mutate(membership = "0") %>%
  rbind(final_child %>%
          dplyr::select(Q52SP02P, Q45COMA2, age2, Q37RESPI2, Q39PALLO2, Malnutrition, Q47DEHYD2, RRcat, HRcat, AdDxMAL, Died) %>%
          mutate(membership = "1")) %>%
  mutate(membership = factor(membership, levels = c("0", "1")))

#XGBoost model
m_model = glm(membership ~ Q52SP02P + Q45COMA2 + age2 + Q37RESPI2 + Q39PALLO2 + Malnutrition+ Died,
              data = comb_data,
              family = "binomial")

#LR model
m_model = glm(membership ~ Q45COMA2 + Malnutrition + Q52SP02P + Q37RESPI2 + AdDxMAL + HRcat+ Died,
              data = comb_data,
              family = "binomial")

#DT model
m_model = glm(membership ~ Q52SP02P + Q45COMA2 + age2 + Q37RESPI2 + Q47DEHYD2 + RRcat+ Died,
              data = comb_data,
              family = "binomial")


pred_res = augment(m_model,
                   type.predict = "response")

pROC::roc(pred_res$membership, pred_res$.fitted)$auc


######Performing external validation for lasso logistic regression######

#Load the model
load("modified_lasso_fit_baggedtree_simpleformula.Rda", verbose = T)

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

#Before recalibration

for (i in unique(final_kenya$hosp_id)) {
  
  #Validation process
  lasso_val = augment(lasso_fit, new_data = final_kenya %>% filter(hosp_id == i))
  
  lasso_val = lasso_val %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  lasso_val <- lasso_val %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  if (i == "8") {
    simpen = lasso_val
  }
  
  if (i != "8") {
    simpen = simpen %>%
      rbind(lasso_val)
  }
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = lasso_val) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = lasso_val)
  
  store[row,1] = pROC::roc(lasso_val$Died, lasso_val$.pred_1)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(lasso_val$Died, lasso_val$.pred_1)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}

#After recalibration

simpen = simpen %>%
  mutate(raw_lp = log(.pred_1/(1-.pred_1)))

logistic_recalibration = glm(Died ~ raw_lp, #Performing logistic recalibration.
                             data = simpen,
                             family = binomial())

simpen = simpen %>%
  mutate(.pred_1_recal = predict(logistic_recalibration, newdata = simpen, type = "response"))

simpen = simpen %>%
  mutate(raw_lp_recal = log(.pred_1_recal/(1-.pred_1_recal)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_kenya$hosp_id)) {
  
  simpen2 = simpen %>%
    filter(hosp_id == i)
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp_recal, 
                  family = binomial, 
                  data = simpen2) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp_recal, 
                      family = binomial,
                      data = simpen2)
  
  store[row,1] = pROC::roc(simpen2$Died, simpen2$.pred_1_recal)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(simpen2$Died, simpen2$.pred_1_recal)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


######Performing external validation for XGBoost######

#Load the models
mod_bundle <- readRDS("final_xgb_bundle.rds")
xgb_fit <- bundle::unbundle(mod_bundle)

#Before recalibration
store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_kenya$hosp_id)) {
  xgb_val = augment(xgb_fit, new_data = final_kenya %>% filter(hosp_id == i))
  
  xgb_val = xgb_val %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  xgb_val <- xgb_val %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  if (i == "8") {
    simpen = xgb_val
  }
  
  if (i != "8") {
    simpen = simpen %>%
      rbind(xgb_val)
  }
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = xgb_val) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = xgb_val)
  
  store[row,1] = pROC::roc(xgb_val$Died, xgb_val$.pred_1)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(xgb_val$Died, xgb_val$.pred_1)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}

#After recalibration
simpen = simpen %>%
  mutate(raw_lp = log(.pred_1/(1-.pred_1)))

logistic_recalibration = glm(Died ~ raw_lp,
                             data = simpen,
                             family = binomial())


simpen = simpen %>%
  mutate(.pred_1_recal = predict(logistic_recalibration, newdata = simpen, type = "response")) #Performing logistic recalibration.

simpen = simpen %>%
  mutate(raw_lp_recal = log(.pred_1_recal/(1-.pred_1_recal)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_kenya$hosp_id)) {
  
  simpen2 = simpen %>%
    filter(hosp_id == i)
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp_recal, 
                  family = binomial, 
                  data = simpen2) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp_recal, 
                      family = binomial,
                      data = simpen2)
  
  store[row,1] = pROC::roc(simpen2$Died, simpen2$.pred_1_recal)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(simpen2$Died, simpen2$.pred_1_recal)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


######Performing external validation for decision tree######

#Load the model
mod_bundle <- readRDS("final_DT_bundle.rds")
tree_fit <- bundle::unbundle(mod_bundle)


final_kenya_dt = final_kenya

#Performing independent imputation for decision tree using KNN imputation model created using external validation dataset.
impute_rec <- recipe(Died ~ 
                       Q52SP02P + Q45COMA2 + age2 + Q37RESPI2 + Q39PALLO2 + Malnutrition + Q47DEHYD2 + RRcat + HRcat + AdDxMAL, 
                     data = final_kenya_dt) %>%
  step_impute_knn(all_predictors())

final_kenya_dt = bake(prep(impute_rec, training = final_kenya_dt), new_data = NULL)

final_kenya_dt = final_kenya_dt %>% 
  mutate(hosp_id = final_kenya$hosp_id,
         Q52SP02P = final_kenya$Q52SP02P)

#Transforming all missing values for SpO2 to be 150 as explained in the method section of the main writing.
final_kenya_dt = final_kenya_dt %>%
  mutate(Q52SP02P = if_else(is.na(Q52SP02P), 150, Q52SP02P))


#These steps are necessary because the decision tree model requires all the candidate predictors to be present as columns in the data frame. 
#When fitting the initial decision tree, we used all 32 candidate predictors. Even though at the end there's 6 final predictors. 
final_child_dt = final_child

final_child_dt[1:nrow(final_kenya_dt),] = NA

data2 = final_kenya_dt %>%
  cbind(final_child_dt %>%
          select(-c(Q52SP02P, Q45COMA2, age2, Q37RESPI2, Q39PALLO2, Malnutrition, Q47DEHYD2, RRcat, HRcat, Died, AdDxMAL, AgeD)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

#Before recalibration
for (i in unique(final_kenya$hosp_id)) {
  
  #Validation process
  tree_val = augment(tree_fit, new_data = data2 %>% dplyr::filter(hosp_id == i))
  
  tree_val = tree_val %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  tree_val <- tree_val %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  if (i == "8") {
    simpen = tree_val
  }
  
  if (i != "8") {
    simpen = simpen %>%
      rbind(tree_val)
  }
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = tree_val) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = tree_val)
  
  store[row,1] = pROC::roc(tree_val$Died, tree_val$.pred_1)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(tree_val$Died, tree_val$.pred_1)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


#After recalibration
simpen = simpen %>%
  mutate(raw_lp = log(.pred_1/(1-.pred_1)))

logistic_recalibration = glm(Died ~ raw_lp,
                             data = simpen,
                             family = binomial())

simpen = simpen %>% #Performing logistic recalibration
  mutate(.pred_1_recal = predict(logistic_recalibration, newdata = simpen, type = "response"))

simpen = simpen %>%
  mutate(raw_lp_recal = log(.pred_1_recal/(1-.pred_1_recal)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_kenya$hosp_id)) {
  
  simpen2 = simpen %>%
    filter(hosp_id == i)
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp_recal, 
                  family = binomial, 
                  data = simpen2) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp_recal, 
                      family = binomial,
                      data = simpen2)
  
  store[row,1] = pROC::roc(simpen2$Died, simpen2$.pred_1_recal)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(simpen2$Died, simpen2$.pred_1_recal)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


######Perfoming analyses for all the results of external validation######

fk = final_kenya 

store = store %>%
  mutate(cluster = as.numeric(cluster)) %>%
  arrange(cluster) %>%
  mutate(cluster = factor(cluster))

#Forest plot for C-statistic (can be used for both prior and after recalibration).
forest(metagen(TE = auc,
               seTE = se,
               studlab = cluster,
               data =  store %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("cluster" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("cluster" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("C-statistic", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       xlim = c(0.5,0.8))

#Forest plot for calibration slop (can be used for both prior and after recalibration).
forest(metagen(TE = slo,
               seTE = slo_se,
               studlab = cluster,
               data =  store %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("cluster" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("cluster" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("Calibration slope", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       xlim = c(0.5,1.7))

#Forst plot for calibration intercept (can be used for both prior and after recalibration).
forest(metagen(TE = int,
               seTE = int_se,
               studlab = cluster,
               data =  store %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("cluster" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("cluster" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("Calibration intercept", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       xlim = c(-1.3,1.3))

#Forest plot for O/E ratio (this is for prior recalibration).
O_E = simpen %>% 
  dplyr::group_by(hosp_id) %>% 
  dplyr::summarize(O = sum(as.numeric(as.character(Died))), E = sum(.pred_1)) %>% mutate(O_E = O/E) %>%
  left_join(simpen %>%
              mutate(mult = .pred_1*(1-.pred_1)) %>%
              select(mult, hosp_id) %>%
              dplyr::group_by(hosp_id) %>%
              dplyr::summarize(mult = sum(mult)),
            by = c("hosp_id" = "hosp_id"))

O_E = O_E %>%
  mutate(sd = 1/E * sqrt(mult))

O_E = O_E %>%
  mutate(hosp_id = factor(hosp_id, levels = store$cluster)) %>%
  arrange(hosp_id)

forest(metagen(TE = O_E,
               seTE = sd,
               studlab = hosp_id,
               data =  O_E %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("hosp_id" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("hosp_id" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("O/E ratio", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       ref = 1,
       xlim = c(0.5, 2.5))

#Forest plot for O/E ratio (this is for after recalibration).
O_E = simpen %>% 
  mutate(.pred_1 = .pred_1_recal) %>%
  dplyr::group_by(hosp_id) %>% 
  dplyr::summarize(O = sum(as.numeric(as.character(Died))), E = sum(.pred_1)) %>% mutate(O_E = O/E) %>%
  left_join(simpen %>%
              mutate(.pred_1 = .pred_1_recal) %>%
              mutate(mult = .pred_1*(1-.pred_1)) %>%
              select(mult, hosp_id) %>%
              dplyr::group_by(hosp_id) %>%
              dplyr::summarize(mult = sum(mult)),
            by = c("hosp_id" = "hosp_id"))

O_E = O_E %>%
  mutate(sd = 1/E * sqrt(mult))

O_E = O_E %>%
  mutate(hosp_id = factor(hosp_id, levels = store$cluster)) %>%
  arrange(hosp_id)

forest(metagen(TE = O_E,
               seTE = sd,
               studlab = hosp_id,
               data =  O_E %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("hosp_id" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("hosp_id" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("O/E ratio", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       ref = 1,
       xlim = c(0, 2))

#Brier score prior to recalibration
simpen %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)


#Brier score after recalibration
simpen %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1_recal)

###Decision curve analysis###

#Compiling all the results prior to recalibration for decision curve analysis
load("XGB_after_onemodel.Rda", verbose = T) #Saved external validation results for simplified XGBoost (sXGB)
simpen_combine_before = simpen

load("LR_EV_simpen_after_simple.Rda") #Saved external validation results for simplified lasso logistic regression (sLR)
simpen_combine_before = simpen_combine_before %>%
  mutate(sLR = simpen$.pred_1)

load("simpen_DT_EV_before.Rda") #Saved external validation results for decision tree
simpen_combine_before = simpen_combine_before %>%
  mutate(DT = simpen$.pred_1)

simpen_combine_before = simpen_combine_before %>%
  rename(c(sXGB = ".pred_1"))

simpen_combine_before = simpen_combine_before %>%
  mutate(`SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0))

#Decision curve analysis for results prior to recalibration
dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
    data = simpen_combine_before,
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT") ~ "Model",
    !(label %in% c("sXGB", "sLR", "DT")) & !(label %in% c("Treat All", "Treat None")) ~ "Individual",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.05774028)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_Coma" = "#030303",
                                "SpO2<85_OR_Coma" = "#888888",
                                "SpO2<90_OR_Coma" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")


#Compiling all the results after recalibration for decision curve analysis
load("XGB_after_onemodel.Rda", verbose = T) #Saved external validation results for simplified XGBoost (sXGB)
simpen_combine_after = simpen

load("LR_EV_simpen_after_simple.Rda", verbose = T) #Saved external validation results for simplified lasso logistic regression (sLR)
simpen_combine_after = simpen_combine_after %>%
  mutate(sLR = simpen$.pred_1_recal)

load("DT_after_onemodel.Rda", verbose = T) #Saved external validation results for decision tree.
simpen_combine_after = simpen_combine_after %>%
  mutate(DT = simpen$.pred_1_recal)

simpen_combine_after = simpen_combine_after %>%
  rename(c(sXGB = ".pred_1_recal"))

simpen_combine_after = simpen_combine_after %>%
  mutate(`SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0))


#Decision curve analysis for results after recalibration
dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
    data = simpen_combine_after,
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT") ~ "Model",
    !(label %in% c("sXGB", "sLR", "DT")) & !(label %in% c("Treat All", "Treat None")) ~ "Individual",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.05774028)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_Coma" = "#030303",
                                "SpO2<85_OR_Coma" = "#888888",
                                "SpO2<90_OR_Coma" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#Decision curve analysis for SpO2 by itself#

final_kenya = final_kenya %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0)) 

#SpO2 only.
dca(Died ~ `SpO2<80` + `SpO2<85` + `SpO2<90`, 
    data = final_kenya %>%
      mutate(Died = factor(Died, levels = c("0", "1"))),
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("Treat All", "Treat None") ~ "Default",
    !(label %in% c("Treat All", "Treat None")) ~ "Individual"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.04)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("SpO2<80" = "#030303", 
                                "SpO2<85" = "#888888",
                                "SpO2<90" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")


######Subgroup analysis based on Sex######
###Prior recalibration###
#Load as needed
load("simpen_xgb_EV_before.Rda", verbose = T) #For sXGBoost
load("LR_EV_simpen_after_simple.Rda", verbose = T) #For sLR
load("simpen_DT_EV_before.Rda", verbose = T) #For DT

#This loop would print out the C-statistic, calibration slope, calibration intercept, and O/E ratio for each subgroup.
for (i in c("Female", "Male")) {
  print(i)
  
  dat = simpen %>%
    filter(child_sex == i) %>%
    mutate(Died = factor(Died, levels = c("0", "1")))
  
  #C-stat
  c_se = sqrt(pROC::var(pROC::roc(dat$Died, dat$.pred_1)))
  print("C-stat")
  print(pROC::roc(dat$Died, dat$.pred_1)$auc)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc - 1.96 * c_se)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc + 1.96 * c_se)
  
  
  
  #Calibration slope
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = dat)
  
  
  print("CS")
  print(coef(summary(slope_pc))[2,1])
  print(coef(summary(slope_pc))[2,1] - 1.96 * coef(summary(slope_pc))[2,2])
  print(coef(summary(slope_pc))[2,1] + 1.96 * coef(summary(slope_pc))[2,2])
  
  
  #Calibration intercept
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = dat)
  
  print("CI") 
  print(coef(summary(intercept_pc))[1,1])
  print(coef(summary(intercept_pc))[1,1] - 1.96 * coef(summary(intercept_pc))[1,2])
  print(coef(summary(intercept_pc))[1,1] + 1.96 * coef(summary(intercept_pc))[1,2])
  
  #O/E ratio
  dat = dat %>%
    mutate(mult = .pred_1*(1-.pred_1))
  
  O = sum(as.numeric(as.character(dat$Died)))
  E = sum(dat$.pred_1)
  O_E = O/E
  mult = sum(dat$mult)
  sd = (1/E) * sqrt(mult)
  
  
  print("O/E")
  print(O_E)
  print(O_E - 1.96*sd)
  print(O_E + 1.96*sd)
  
}

#Brier Class for female
simpen %>%
  filter(child_sex == "Female") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)

#Brier Class for male
simpen %>%
  filter(child_sex == "Male") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)

#Compiling all the results for decision curve analysis
load("XGB_after_onemodel.Rda", verbose = T) #sXGB
simpen_combine_before = simpen

load("LR_EV_simpen_after_simple.Rda") #sLR
simpen_combine_before = simpen_combine_before %>%
  mutate(sLR = simpen$.pred_1)

load("simpen_DT_EV_before.Rda") #DT
simpen_combine_before = simpen_combine_before %>%
  mutate(DT = simpen$.pred_1)

simpen_combine_before = simpen_combine_before %>%
  rename(c(sXGB = ".pred_1"))

simpen_combine_before = simpen_combine_before %>%
  mutate(`SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0))

#Decision curve analysis for female
fem = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
          data = simpen_combine_before %>%
            filter(child_sex == "Female"),
          thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT") ~ "Model",
    !(label %in% c("sXGB", "sLR", "DT")) & !(label %in% c("Treat All", "Treat None")) ~ "Individual",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_Coma" = "#030303",
                                "SpO2<85_OR_Coma" = "#888888",
                                "SpO2<90_OR_Coma" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#Decision curve analysis for male
mal = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
          data = simpen_combine_before %>%
            filter(child_sex == "Male"),
          thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT") ~ "Model",
    !(label %in% c("sXGB", "sLR", "DT")) & !(label %in% c("Treat All", "Treat None")) ~ "Individual",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_Coma" = "#030303",
                                "SpO2<85_OR_Coma" = "#888888",
                                "SpO2<90_OR_Coma" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

###After recalibration###
#Load as needed.
load("XGB_after_onemodel.Rda", verbose = T) #For sXGB
load("LR_EV_simpen_after_simple.Rda", verbose = T) #For sLR
load("DT_after_onemodel.Rda", verbose = T) #For DT

for (i in c("Female", "Male")) {
  print(i)
  
  dat = simpen %>%
    filter(child_sex == i) %>%
    mutate(Died = factor(Died, levels = c("0", "1"))) %>%
    mutate(.pred_1 = .pred_1_recal, #In the recalibrated dataset, the recalibrated predicted probability and linear predictor has .pred_1_recal and raw_lp_recal as its name, respectively. 
           raw_lp = raw_lp_recal) #They were both transformed so we could use existing coude without changing everything.
  
  #C-stat
  c_se = sqrt(pROC::var(pROC::roc(dat$Died, dat$.pred_1)))
  print("C-stat")
  print(pROC::roc(dat$Died, dat$.pred_1)$auc)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc - 1.96 * c_se)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc + 1.96 * c_se)
  
  
  
  #Calibration slope
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = dat)
  
  
  print("CS")
  print(coef(summary(slope_pc))[2,1])
  print(coef(summary(slope_pc))[2,1] - 1.96 * coef(summary(slope_pc))[2,2])
  print(coef(summary(slope_pc))[2,1] + 1.96 * coef(summary(slope_pc))[2,2])
  
  
  #Calibration intercept
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = dat)
  
  print("CI") 
  print(coef(summary(intercept_pc))[1,1])
  print(coef(summary(intercept_pc))[1,1] - 1.96 * coef(summary(intercept_pc))[1,2])
  print(coef(summary(intercept_pc))[1,1] + 1.96 * coef(summary(intercept_pc))[1,2])
  
  #O/E ratio
  dat = dat %>%
    mutate(mult = .pred_1*(1-.pred_1))
  
  O = sum(as.numeric(as.character(dat$Died)))
  E = sum(dat$.pred_1)
  O_E = O/E
  mult = sum(dat$mult)
  sd = (1/E) * sqrt(mult)
  
  
  print("O/E")
  print(O_E)
  print(O_E - 1.96*sd)
  print(O_E + 1.96*sd)
  
}

#Brier Class for female
simpen %>%
  filter(child_sex == "Female") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1_recal)

#Brier Class for male
simpen %>%
  filter(child_sex == "Male") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1_recal)

#Compiling all the results for decision curve analysis
load("XGB_after_onemodel.Rda", verbose = T)
simpen_combine_after = simpen

load("LR_EV_simpen_after_simple.Rda", verbose = T)
simpen_combine_after = simpen_combine_after %>%
  mutate(sLR = simpen$.pred_1_recal)

load("DT_after_onemodel.Rda", verbose = T)
simpen_combine_after = simpen_combine_after %>%
  mutate(DT = simpen$.pred_1_recal)

simpen_combine_after = simpen_combine_after %>%
  rename(c(sXGB = ".pred_1_recal"))

simpen_combine_after = simpen_combine_after %>%
  mutate(`SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0))

#Decision curve analysis for female
fem2 = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
           data = simpen_combine_after %>%
             filter(child_sex == "Female"),
           thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT") ~ "Model",
    !(label %in% c("sXGB", "sLR", "DT")) & !(label %in% c("Treat All", "Treat None")) ~ "Individual",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_Coma" = "#030303",
                                "SpO2<85_OR_Coma" = "#888888",
                                "SpO2<90_OR_Coma" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#Decision curve analysis for male
mal2 = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
           data = simpen_combine_after %>%
             filter(child_sex == "Male"),
           thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT") ~ "Model",
    !(label %in% c("sXGB", "sLR", "DT")) & !(label %in% c("Treat All", "Treat None")) ~ "Individual",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_Coma" = "#030303",
                                "SpO2<85_OR_Coma" = "#888888",
                                "SpO2<90_OR_Coma" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

ggarrange(fem, mal, fem2, mal2, nrow = 2, ncol = 2, common.legend = T) #Combine decision curves.

