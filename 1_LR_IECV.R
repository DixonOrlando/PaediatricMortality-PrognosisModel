###IECV for lasso logistic regression###

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

###########PREPARATION FOR MICE IMPUTATION###########################

####GETTING NON-LINEARITY###

results = data.frame(var1 = "A", p_val = 1, var2 = "B")
row = 1

#The HRcat, RRcat, and agecat should not be analysed using logistic regression!

#Instead we impute them in continuous form!

#Rescale and center prior to fitting the mixed model, so the model is stable.

final_child_res = final_child

final_child_res[sapply(final_child_res , is.numeric)] <- scale(final_child_res [sapply(final_child_res , is.numeric)])

for (i in final_child %>% select(Q52SP02P, Q51RESPI, Q50HEART, Q12SEX) %>% names()) {
  
  if (i == "Q12SEX") {
    form_A1 = as.formula(paste(i, paste("Q52SP02P", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_A2 = as.formula(paste(i, paste("Q52SP02P", " I(Q52SP02P^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_A1, 
                                  data = final_child_res, 
                                  family = binomial(), 
                                  nAGQ=30) , glmer(form_A2, 
                                                   data = final_child_res, 
                                                   family = binomial(),
                                                   nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q52SP02P"
    row = row + 1
    
    
    form_B1 = as.formula(paste(i, paste("Q51RESPI", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_B2 = as.formula(paste(i, paste("Q51RESPI", " I(Q51RESPI^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_B1, 
                                  data = final_child_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_B2, 
                                                                        data = final_child_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q51RESPI"
    row = row + 1
    
    form_C1 = as.formula(paste(i, paste("Q50HEART", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_C2 = as.formula(paste(i, paste("Q50HEART", " I(Q50HEART^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_C1, 
                                  data = final_child_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_C2, 
                                                                        data = final_child_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q50HEART"
    row = row + 1
    
    form_D1 = as.formula(paste(i, paste("age2", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_D2 = as.formula(paste(i, paste("age2", " I(age2^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_D1, 
                                  data = final_child_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_D2, 
                                                                        data = final_child_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "age2"
    row = row + 1
  }
  
  else {
    
    if (i != "Q52SP02P") {
      form_A1 = as.formula(paste(i, paste("Q52SP02P", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
      form_A2 = as.formula(paste(i, paste("Q52SP02P", " I(Q52SP02P^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
      
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_A1, 
                                   data = final_child_res) , lmer(form_A2, 
                                                                  data = final_child_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q52SP02P"
      row = row + 1
      
      
    }
    
    if (i != "Q51RESPI") {
      form_B1 = as.formula(paste(i, paste("Q51RESPI", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
      form_B2 = as.formula(paste(i, paste("Q51RESPI", " I(Q51RESPI^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
      
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_B1, 
                                   data = final_child_res) , lmer(form_B2, 
                                                                  data = final_child_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q51RESPI"
      row = row + 1
    }
    
    if (i != "Q50HEART") {
      form_C1 = as.formula(paste(i, paste("Q50HEART", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
      form_C2 = as.formula(paste(i, paste("Q50HEART", " I(Q50HEART^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
      
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_C1, 
                                   data = final_child_res) , lmer(form_C2, 
                                                                  data = final_child_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q50HEART"
      row = row + 1
    }
    
    
  }
  
  if (i == "Q12SEX") {
    form_D1 = as.formula(paste("Q52SP02P", paste("age2", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_D2 = as.formula(paste("Q52SP02P", paste("age2", " I(age2^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = "Q52SP02P"
    results[row, 2] = anova(lmer(form_D1, 
                                 data = final_child_res) , lmer(form_D2, 
                                                                data = final_child_res))$`Pr(>Chisq)`[2]
    results[row, 3] = "age2"
    row = row + 1
    
    form_D1 = as.formula(paste("Q51RESPI", paste("age2", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_D2 = as.formula(paste("Q51RESPI", paste("age2", " I(age2^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = "Q51RESPI"
    results[row, 2] = anova(lmer(form_D1, 
                                 data = final_child_res) , lmer(form_D2, 
                                                                data = final_child_res))$`Pr(>Chisq)`[2]
    results[row, 3] = "age2"
    row = row + 1
    
    form_D1 = as.formula(paste("Q50HEART", paste("age2", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    form_D2 = as.formula(paste("Q50HEART", paste("age2", " I(age2^2)", " (1|Q005HEAL)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = "Q50HEART"
    results[row, 2] = anova(lmer(form_D1, 
                                 data = final_child_res) , lmer(form_D2, 
                                                                data = final_child_res))$`Pr(>Chisq)`[2]
    results[row, 3] = "age2"
    row = row + 1
  }
  
  
  
}

###MICE IMPUTATION FUNCTION###

mice_imp <- function(train, test, id) {
  
  train = train %>%
    mutate(agecat = case_when(
      agecat == "Age<1" ~ "1",
      agecat == "1<=Age<2" ~ "2",
      agecat == "2<=Age<5" ~ "3",
      agecat == "5<=Age<13" ~ "4"
    ),
    agecat = factor(agecat, levels = c("1", "2", "3", "4")),
    age2 = as.numeric(age2))
  
  if (id != "Full") {
    test = test %>%
      mutate(agecat = case_when(
        agecat == "Age<1" ~ "1",
        agecat == "1<=Age<2" ~ "2",
        agecat == "2<=Age<5" ~ "3",
        agecat == "5<=Age<13" ~ "4"
      ),
      agecat = factor(agecat, levels = c("1", "2", "3", "4")),
      age2 = as.numeric(age2)) 
  }
  
  #Storing the scaling value so it can be used for reverting the scaled data back to original scale.
  scaling_params_train <- train  %>%
    summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_")
  
  if (id != "Full") {
    scaling_params_test <- test  %>%
      summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_")
  }
  
  
  #Scaling the data prior to imputation
  train[sapply(train , is.numeric)] <- scale(train [sapply(train , is.numeric)]) 
  
  if (id != "Full") {
    test[sapply(test , is.numeric)] <- scale(test [sapply(test , is.numeric)])
  }
  
  
  #Starting the MICE imputation process.
  imp <- mice(train %>% 
                select(Q005HEAL, Q12SEX, Q35FEVER2, Q36COUGH2, Q41DIARH2, Q37RESPI2, Q38CYANO2, Q39PALLO2, Q42UNBRE2, Q43CONVU2, Q44CONFU2, Q45COMA2, Q46SIGNO2, Q47DEHYD2, Malnutrition, AdDxLRTI, AdDxMAL, AdDxDIAR, AdDxSEPTIC, AdDxURTI, AdDxASTH, AdDxTYPH, AdDxHIV, AdDxSEIZ, AdDxMENG, AdDxHAEM, AdDxSKIN, AdDxTRAUMA, AdDxABDO, Q50HEART, Q51RESPI, agecat, Q52SP02P, Died, age2, FullOxygen) %>%
                mutate(Q52SP02P_sq = Q52SP02P^2,
                       Q50HEART_sq = Q50HEART^2,
                       Q51RESPI_sq = Q51RESPI^2,
                       age2_sq = age2^2) 
              ,printFlag = FALSE,
              maxit = 0) # Dry run
  
  pred <- imp$predictorMatrix
  
  pred[pred == 1] <- 1 # Accounts for between-study heterogeneity in the effect of all variables; 2 actually implies a random effect.
  
  pred[, "Q52SP02P_sq"] = 0
  pred["Q52SP02P_sq", "Q52SP02P"] = 0
  
  pred[, "Q50HEART_sq"] = 0
  pred["Q50HEART_sq", "Q50HEART"] = 0
  
  pred[, "Q51RESPI_sq"] = 0
  pred["Q51RESPI_sq", "Q51RESPI"] = 0
  
  pred[, "age2_sq"] = 0
  pred["age2_sq", "age2"] = 0
  
  pred[, "agecat"] = 0
  
  
  pred[, "Q005HEAL"] <- -2 # Specify the cluster variable to allow random effects on the intercept term across each practice
  
  results_filt = results %>%
    filter(p_val <= 0.05)
  
  
  
  for (i in 1:nrow(results_filt)) {
    pred[results_filt[i,1], paste(results_filt[i,3], "_sq", sep="")] = 1
  }
  
  
  
  meth <- imp$method
  
  meth[meth == "pmm"] <- "2l.pmm" # Multi-level predictive mean matching for continuous variables
  
  meth[meth == "logreg"] <- "2l.pmm" # Multi-level logistic regression for binary variables
  
  meth["Q52SP02P_sq"] <- "~ I(Q52SP02P^2)"
  meth["Q50HEART_sq"] <- "~ I(Q50HEART^2)"
  meth["Q51RESPI_sq"] <- "~ I(Q51RESPI^2)"
  meth["age2_sq"] <- "~ I(age2^2)"
  
  
  mice <- mice(train %>%
                 select(Q005HEAL, Q12SEX, Q35FEVER2, Q36COUGH2, Q41DIARH2, Q37RESPI2, Q38CYANO2, Q39PALLO2, Q42UNBRE2, Q43CONVU2, Q44CONFU2, Q45COMA2, Q46SIGNO2, Q47DEHYD2, Malnutrition, AdDxLRTI, AdDxMAL, AdDxDIAR, AdDxSEPTIC, AdDxURTI, AdDxASTH, AdDxTYPH, AdDxHIV, AdDxSEIZ, AdDxMENG, AdDxHAEM, AdDxSKIN, AdDxTRAUMA, AdDxABDO, Q50HEART, Q51RESPI, agecat, Q52SP02P, Died, age2, FullOxygen) %>%
                 mutate(Q005HEAL = as.integer(Q005HEAL)) %>%
                 mutate(Q52SP02P_sq = Q52SP02P^2,
                        Q50HEART_sq = Q50HEART^2,
                        Q51RESPI_sq = Q51RESPI^2,
                        age2_sq = age2^2) 
               , m = 1, maxit = 10, meth = meth, pred = pred, 
               seed = 1111)
  
  if (id != "Full") {
    mice_test = mice.mids(mice, 
                          newdata = test %>%
                            select(Q005HEAL, Q12SEX, Q35FEVER2, Q36COUGH2, Q41DIARH2, Q37RESPI2, Q38CYANO2, Q39PALLO2, Q42UNBRE2, Q43CONVU2, Q44CONFU2, Q45COMA2, Q46SIGNO2, Q47DEHYD2, Malnutrition, AdDxLRTI, AdDxMAL, AdDxDIAR, AdDxSEPTIC, AdDxURTI, AdDxASTH, AdDxTYPH, AdDxHIV, AdDxSEIZ, AdDxMENG, AdDxHAEM, AdDxSKIN, AdDxTRAUMA, AdDxABDO, Q50HEART, Q51RESPI, agecat, Q52SP02P, Died, age2, FullOxygen) %>%
                            mutate(Q005HEAL = as.integer(Q005HEAL)) %>%
                            mutate(Q52SP02P_sq = Q52SP02P^2,
                                   Q50HEART_sq = Q50HEART^2,
                                   Q51RESPI_sq = Q51RESPI^2,
                                   age2_sq = age2^2) , 
                          maxit = 1, 
                          seed = 1111)
  }
  
  #For the training data:
  
  final_train = complete(mice) %>%
    select(-c(Q52SP02P_sq, Q50HEART_sq, Q51RESPI_sq, age2_sq))
  
  Q005HEAL_store = final_train$Q005HEAL
  
  #Convert back to original scale
  final_train = final_train %>%
    mutate(across(where(is.numeric), ~ .x * scaling_params_train$sd[match(cur_column(), scaling_params_train$variable)] +
                    scaling_params_train$mean[match(cur_column(), scaling_params_train$variable)])) %>%
    mutate(Q005HEAL = Q005HEAL_store) %>%
    mutate(tt_id = 1,
           index = id, 
           cluster_code = train$cluster_code) %>%
    mutate (HRcat = case_when( 
      
      is.na(Q50HEART) ~ NA_character_, 
      
      Q50HEART < 100 & age2 <12 ~ "Low", 
      
      Q50HEART < 80 & age2 %in% c(12:60) ~ "Low", 
      
      Q50HEART < 70 & age2 >59.9 ~ "Low", 
      
      Q50HEART > 160 & age2 <12 ~ "High", 
      
      Q50HEART > 150 & age2 %in% c(12:60) ~ "High", 
      
      Q50HEART > 130 & age2 >59.9 ~ "High", 
      
      TRUE ~ "Normal")) %>% 
    
    mutate(HRcat= factor(HRcat, levels = levels(final_child$HRcat))) %>%
    
    mutate (RRcat = case_when( 
      
      is.na(Q51RESPI) ~ NA_character_, 
      
      Q51RESPI < 25 & age2 <12 ~ "Low", 
      
      Q51RESPI < 20 & age2 %in% c(12:60) ~ "Low", 
      
      Q51RESPI < 15 & age2 >59.9 ~ "Low", 
      
      Q51RESPI > 50 & age2 <12 ~ "High", 
      
      Q51RESPI > 40 & age2 %in% c(12:60) ~ "High", 
      
      Q51RESPI > 30 & age2 >59.9 ~ "High", 
      
      TRUE ~ "Normal")) %>% 
    
    mutate(RRcat= factor(RRcat, levels = levels(final_child$RRcat))) %>% 
    
    mutate(agecat = case_when(
      agecat == "1" ~ "Age<1",
      agecat == "2" ~ "1<=Age<2",
      agecat == "3" ~ "2<=Age<5",
      agecat == "4" ~ "5<=Age<13"
    ),
    agecat = factor(agecat, levels = c("Age<1", "1<=Age<2", "2<=Age<5", "5<=Age<13"))) %>%
    
    mutate(age2 = as.numeric(age2)) %>%
    
    select(-c(Q50HEART, Q51RESPI)) 
  
  if (id != "Full") {
    #For the test data
    final_test = complete(mice_test) %>%
      select(-c(Q52SP02P_sq, Q50HEART_sq, Q51RESPI_sq, age2_sq))
    
    Q005HEAL_store_test = final_test$Q005HEAL
    
    #Convert back to original scale
    final_test = final_test %>%
      mutate(across(where(is.numeric), ~ .x * scaling_params_test$sd[match(cur_column(), scaling_params_test$variable)] +
                      scaling_params_test$mean[match(cur_column(), scaling_params_test$variable)])) %>%
      mutate(Q005HEAL = Q005HEAL_store_test) %>%
      mutate(tt_id = 2, #2 is for the test data.
             index = id, 
             cluster_code = test$cluster_code) %>%
      
      mutate (HRcat = case_when( 
        
        is.na(Q50HEART) ~ NA_character_, 
        
        Q50HEART < 100 & age2 <12 ~ "Low", 
        
        Q50HEART < 80 & age2 %in% c(12:60) ~ "Low", 
        
        Q50HEART < 70 & age2 >59.9 ~ "Low", 
        
        Q50HEART > 160 & age2 <12 ~ "High", 
        
        Q50HEART > 150 & age2 %in% c(12:60) ~ "High", 
        
        Q50HEART > 130 & age2 >59.9 ~ "High", 
        
        TRUE ~ "Normal")) %>% 
      
      mutate(HRcat= factor(HRcat, levels = levels(final_child$HRcat))) %>% 
      
      mutate (RRcat = case_when( 
        
        is.na(Q51RESPI) ~ NA_character_, 
        
        Q51RESPI < 25 & age2 <12 ~ "Low", 
        
        Q51RESPI < 20 & age2 %in% c(12:60) ~ "Low", 
        
        Q51RESPI < 15 & age2 >59.9 ~ "Low", 
        
        Q51RESPI > 50 & age2 <12 ~ "High", 
        
        Q51RESPI > 40 & age2 %in% c(12:60) ~ "High", 
        
        Q51RESPI > 30 & age2 >59.9 ~ "High", 
        
        TRUE ~ "Normal")) %>% 
      
      mutate(RRcat= factor(RRcat, levels = levels(final_child$RRcat))) %>% 
      
      mutate(age2 = as.numeric(age2)) %>%
      
      select(-c(Q50HEART, Q51RESPI)) %>%
      
      mutate(agecat = case_when(
        agecat == "1" ~ "Age<1",
        agecat == "2" ~ "1<=Age<2",
        agecat == "3" ~ "2<=Age<5",
        agecat == "4" ~ "5<=Age<13"
      ),
      agecat = factor(agecat, levels = c("Age<1", "1<=Age<2", "2<=Age<5", "5<=Age<13")))
    
    final_data = final_train %>% rbind(final_test)
    
    return(final_data)
  }
  
  if (id == "Full") {
    return(final_train)
  }
  
}


######################Lasso logistic regression with bagged imputation; prior simplification#####################
for (i in unique(final_child$cluster_code)) {
  
  if (i == "1"){
    
    index = 1
    row = 1
    
    metric_measure_clas_imp <- data.frame(pr_auc = 1,
                                          roc_auc = 1,
                                          log_loss = 1,
                                          brier = 1,
                                          cluster_code = "A")
    
    calibration_measure_imp <- data.frame(cal_slope = 1,
                                          cal_int = 1,
                                          
                                          se_slope = 1,
                                          
                                          se_int = 1,
                                          cluster_code = "A")
    
    c_stat_imp <- data.frame(roc_auc = 1,
                             se_roc_auc = 1,
                             cluster_code = "A")
    
    mn_cal <- list(pred = 1, obs = 1)
    
  }
  
  child_train1 <- final_child %>% #Continuous specification
    filter(cluster_code != i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_test1 <- final_child %>%
    filter(cluster_code == i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_rec <- recipe(Died ~ Q12SEX + Q35FEVER2 + Q36COUGH2 + Q37RESPI2 + Q38CYANO2 + Q39PALLO2 + Q42UNBRE2 + Q43CONVU2 + Q44CONFU2 + Q45COMA2 + Q46SIGNO2 + Q47DEHYD2 + Malnutrition + AdDxLRTI + AdDxMAL + AdDxDIAR + AdDxSEPTIC + AdDxURTI + AdDxASTH + AdDxTYPH + AdDxHIV + AdDxSEIZ + AdDxMENG + AdDxHAEM + AdDxSKIN + AdDxTRAUMA + AdDxABDO + HRcat + RRcat + age2 + Q52SP02P + FullOxygen, data = child_train1) %>%
    step_impute_bag(all_predictors()) %>%
    step_poly(all_numeric_predictors(), degree = tune()) %>%
    step_center(all_numeric_predictors(), -all_outcomes()) %>%
    step_scale(all_numeric_predictors(), -all_outcomes(), factor = 2) %>%
    step_dummy(all_nominal(), -all_outcomes()) 
  
  wf <- workflow() %>%
    add_recipe(child_rec)
  
  set.seed(1111)
  child_boot <- vfold_cv(child_train1, strata = Died, v = 5)
  
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
  
  set.seed(1111)
  
  lasso_fit <- fit(final_lasso, data = child_train1)
  
  cell_test_pred <- augment(lasso_fit, new_data = child_test1)
  
  cell_test_pred <- cell_test_pred %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  cell_test_pred <- cell_test_pred %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = cell_test_pred) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = cell_test_pred)
  
  metric_measure_clas_imp[row, 1] <- (cell_test_pred %>% pr_auc(Died, .pred_1))[3] #PR-AUC
  metric_measure_clas_imp[row, 2] <- (cell_test_pred %>% roc_auc(Died, .pred_1))[3] #ROC-AUC
  metric_measure_clas_imp[row, 3] <- (cell_test_pred %>% mn_log_loss(truth = Died, .pred_1))[3] #log-loss
  metric_measure_clas_imp[row, 4] <-  (cell_test_pred %>% brier_class(truth = Died, .pred_1))[3] #Brier
  metric_measure_clas_imp[row, 5] <- i
  
  calibration_measure_imp[row, 1] <- coef(summary(slope_pc))[2,1] #cal_slope
  calibration_measure_imp[row, 2] <- coef(summary(intercept_pc))[1,1] #cal_int
  calibration_measure_imp[row, 3] <- coef(summary(slope_pc))[2,2] #se_slope
  calibration_measure_imp[row, 4] <- coef(summary(intercept_pc))[1,2]#se_int
  calibration_measure_imp[row, 5] <- i #cluster_code
  
  c_stat_imp[row, 1] <- pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)$auc
  c_stat_imp[row, 2] <- sqrt(var(pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)))
  c_stat_imp[row, 3] <- i
  
  
  if (i == "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- p
    mn_cal$obs <- y
    
    simpen <- cell_test_pred
  }
  
  if (i != "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- c(mn_cal$pred, p)
    mn_cal$obs <- c(mn_cal$obs, y)
    
    simpen <- rbind(simpen, cell_test_pred)
  }
  
  
  
  row = row + 1
}


######################Lasso logistic regression with MICE imputation; prior simplification#####################
for (i in unique(final_child$cluster_code)) {
  
  if (i == "1"){
    
    index = 1
    row = 1
    
    metric_measure_clas_imp <- data.frame(pr_auc = 1,
                                          roc_auc = 1,
                                          log_loss = 1,
                                          brier = 1,
                                          cluster_code = "A")
    
    calibration_measure_imp <- data.frame(cal_slope = 1,
                                          cal_int = 1,
                                          
                                          se_slope = 1,
                                          
                                          se_int = 1,
                                          cluster_code = "A")
    
    c_stat_imp <- data.frame(roc_auc = 1,
                             se_roc_auc = 1,
                             cluster_code = "A")
    
    mn_cal <- list(pred = 1, obs = 1)
    
  }
  
  child_train1 <- final_child %>% #Continuous specification
    filter(cluster_code != i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_test1 <- final_child %>%
    filter(cluster_code == i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_rec <- recipe(Died ~ Q12SEX + Q35FEVER2 + Q36COUGH2 + Q37RESPI2 + Q38CYANO2 + Q39PALLO2 + Q42UNBRE2 + Q43CONVU2 + Q44CONFU2 + Q45COMA2 + Q46SIGNO2 + Q47DEHYD2 + Malnutrition + AdDxLRTI + AdDxMAL + AdDxDIAR + AdDxSEPTIC + AdDxURTI + AdDxASTH + AdDxTYPH + AdDxHIV + AdDxSEIZ + AdDxMENG + AdDxHAEM + AdDxSKIN + AdDxTRAUMA + AdDxABDO + HRcat + RRcat + age2 + Q52SP02P + FullOxygen, data = child_train1) %>%
    step_poly(all_numeric_predictors(), degree = tune()) %>%
    step_center(all_numeric_predictors(), -all_outcomes()) %>%
    step_scale(all_numeric_predictors(), -all_outcomes(), factor = 2) %>%
    step_dummy(all_nominal(), -all_outcomes()) 
  
  wf <- workflow() %>%
    add_recipe(child_rec)
  
  set.seed(1111)
  child_boot <- vfold_cv(child_train1, strata = Died, v = 5)
  
  ####LOOP TO PERFORM IMPUTATION INSIDE THE VFOLD CV####
  
  for (k in 1:5){
    df_train <- get_rsplit(child_boot, index = k) %>%
      analysis() 
    
    df_test <- get_rsplit(child_boot, index = k) %>%
      assessment() 
    
    if (k == 1) {
      boot_data = mice_imp(train = df_train, test = df_test, id = k)
    }
    
    if (k == 2) {
      add_boot = mice_imp(train = df_train, test = df_test, id = k)
      
      boot_data = boot_data %>%
        rbind(add_boot)
      
      a = list(analysis = which(boot_data$tt_id == 1 & boot_data$index == 1)[1] : which(boot_data$tt_id == 1 & boot_data$index == 1)[length(which(boot_data$tt_id == 1 & boot_data$index == 1))],
               assessment = which(boot_data$tt_id == 2 & boot_data$index == 1)[1] : which(boot_data$tt_id == 2 & boot_data$index == 1)[length(which(boot_data$tt_id == 2 & boot_data$index == 1))]
      )
      
      b = list(analysis = which(boot_data$tt_id == 1 & boot_data$index == 2)[1] : which(boot_data$tt_id == 1 & boot_data$index == 2)[length(which(boot_data$tt_id == 1 & boot_data$index == 2))],
               assessment = which(boot_data$tt_id == 2 & boot_data$index == 2)[1] : which(boot_data$tt_id == 2 & boot_data$index == 2)[length(which(boot_data$tt_id == 2 & boot_data$index == 2))]
      )
      
      indices = append(list(a), list(b))
    }
    
    if (k > 2) {
      boot_data = boot_data %>%
        rbind(mice_imp(train = df_train, test = df_test, id = k))
      
      c = list(analysis = which(boot_data$tt_id == 1 & boot_data$index == k)[1] : which(boot_data$tt_id == 1 & boot_data$index == k)[length(which(boot_data$tt_id == 1 & boot_data$index == k))],
               assessment = which(boot_data$tt_id == 2 & boot_data$index == k)[1] : which(boot_data$tt_id == 2 & boot_data$index == k)[length(which(boot_data$tt_id == 2 & boot_data$index == k))]
      )
      
      indices = indices %>%
        append(list(c))
    }
    
    
  }
  
  splits <- lapply(indices, make_splits, data = boot_data)
  
  child_boot <- manual_rset(splits, c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5"))
  
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
  
  #####PERFORMING MICE IMPUTATION ON THE FULL DATA########
  
  full_imp <- mice_imp(train = child_train1,
                       test = child_test1,
                       id = "Full Data")
  
  child_train1 <- full_imp %>%
    filter(cluster_code != i) 
  
  child_test1 <- full_imp %>%
    filter(cluster_code == i)
  
  ########################################################
  
  set.seed(1111)
  
  lasso_fit <- fit(final_lasso, data = child_train1)
  
  cell_test_pred <- augment(lasso_fit, new_data = child_test1)
  
  cell_test_pred <- cell_test_pred %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  cell_test_pred <- cell_test_pred %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = cell_test_pred) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = cell_test_pred)
  
  metric_measure_clas_imp[row, 1] <- (cell_test_pred %>% pr_auc(Died, .pred_1))[3] #PR-AUC
  metric_measure_clas_imp[row, 2] <- (cell_test_pred %>% roc_auc(Died, .pred_1))[3] #ROC-AUC
  metric_measure_clas_imp[row, 3] <- (cell_test_pred %>% mn_log_loss(truth = Died, .pred_1))[3] #log-loss
  metric_measure_clas_imp[row, 4] <-  (cell_test_pred %>% brier_class(truth = Died, .pred_1))[3] #Brier
  metric_measure_clas_imp[row, 5] <- i
  
  calibration_measure_imp[row, 1] <- coef(summary(slope_pc))[2,1] #cal_slope
  calibration_measure_imp[row, 2] <- coef(summary(intercept_pc))[1,1] #cal_int
  calibration_measure_imp[row, 3] <- coef(summary(slope_pc))[2,2] #se_slope
  calibration_measure_imp[row, 4] <- coef(summary(intercept_pc))[1,2]#se_int
  calibration_measure_imp[row, 5] <- i #cluster_code
  
  c_stat_imp[row, 1] <- pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)$auc
  c_stat_imp[row, 2] <- sqrt(var(pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)))
  c_stat_imp[row, 3] <- i
  
  
  if (i == "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- p
    mn_cal$obs <- y
    
    simpen <- cell_test_pred
  }
  
  if (i != "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- c(mn_cal$pred, p)
    mn_cal$obs <- c(mn_cal$obs, y)
    
    simpen <- rbind(simpen, cell_test_pred)
  }
  
  
  
  row = row + 1
}


######################Lasso logistic regression with bagged tree imputation; after simplification#####################
for (i in unique(final_child$cluster_code)) {
  
  if (i == "1"){
    
    index = 1
    row = 1
    
    metric_measure_clas_imp <- data.frame(pr_auc = 1,
                                          roc_auc = 1,
                                          log_loss = 1,
                                          brier = 1,
                                          cluster_code = "A")
    
    calibration_measure_imp <- data.frame(cal_slope = 1,
                                          cal_int = 1,
                                          
                                          se_slope = 1,
                                          
                                          se_int = 1,
                                          cluster_code = "A")
    
    c_stat_imp <- data.frame(roc_auc = 1,
                             se_roc_auc = 1,
                             cluster_code = "A")
    
    mn_cal <- list(pred = 1, obs = 1)
    
  }
  
  child_train1 <- final_child %>% #Continuous specification
    filter(cluster_code != i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_test1 <- final_child %>%
    filter(cluster_code == i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_rec <- recipe(Died ~ Q45COMA2 + Malnutrition + Q52SP02P + Q37RESPI2 + AdDxMAL + HRcat, data = child_train1) %>%
    step_impute_bag(all_predictors()) %>%
    step_poly(all_numeric_predictors(), degree = tune()) %>%
    step_center(all_numeric_predictors(), -all_outcomes()) %>%
    step_scale(all_numeric_predictors(), -all_outcomes(), factor = 2) %>%
    step_dummy(all_nominal(), -all_outcomes()) 
  
  wf <- workflow() %>%
    add_recipe(child_rec)
  
  set.seed(1111)
  child_boot <- vfold_cv(child_train1, strata = Died, v = 5)
  
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
  
  set.seed(1111)
  
  lasso_fit <- fit(final_lasso, data = child_train1)
  
  cell_test_pred <- augment(lasso_fit, new_data = child_test1)
  
  cell_test_pred <- cell_test_pred %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  cell_test_pred <- cell_test_pred %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = cell_test_pred) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = cell_test_pred)
  
  metric_measure_clas_imp[row, 1] <- (cell_test_pred %>% pr_auc(Died, .pred_1))[3] #PR-AUC
  metric_measure_clas_imp[row, 2] <- (cell_test_pred %>% roc_auc(Died, .pred_1))[3] #ROC-AUC
  metric_measure_clas_imp[row, 3] <- (cell_test_pred %>% mn_log_loss(truth = Died, .pred_1))[3] #log-loss
  metric_measure_clas_imp[row, 4] <-  (cell_test_pred %>% brier_class(truth = Died, .pred_1))[3] #Brier
  metric_measure_clas_imp[row, 5] <- i
  
  calibration_measure_imp[row, 1] <- coef(summary(slope_pc))[2,1] #cal_slope
  calibration_measure_imp[row, 2] <- coef(summary(intercept_pc))[1,1] #cal_int
  calibration_measure_imp[row, 3] <- coef(summary(slope_pc))[2,2] #se_slope
  calibration_measure_imp[row, 4] <- coef(summary(intercept_pc))[1,2]#se_int
  calibration_measure_imp[row, 5] <- i #cluster_code
  
  c_stat_imp[row, 1] <- pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)$auc
  c_stat_imp[row, 2] <- sqrt(var(pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)))
  c_stat_imp[row, 3] <- i
  
  
  if (i == "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- p
    mn_cal$obs <- y
    
    simpen <- cell_test_pred
  }
  
  if (i != "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- c(mn_cal$pred, p)
    mn_cal$obs <- c(mn_cal$obs, y)
    
    simpen <- rbind(simpen, cell_test_pred)
  }
  
  
  
  row = row + 1
}






