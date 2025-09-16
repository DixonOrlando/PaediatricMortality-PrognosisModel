###Model evaluation###

###Loading the packages###
library(tidyverse)
library(meta)
library(dcurves)
library(tidymodels)
library(CalibrationCurves)
library(pmcalibration)
library(dmetar)
library(MuMIn)
library(metafor)
library(dmetar)
library(netmeta)
library(rmda)
library(grid)
library(moments)

##############################Reading the data##############################

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

##############################Only lasso logistic regression is used as an example/demonstration for this code##############################

#The reason we do not provide examples for all the models because there are too many of them. However, the other models could use the same code below to evaluate its performance.

###Load all the results for the simplified lasso logistic regression (sLR) in the IECV###
load("modifiedSP_cal_lasso_regular_baggedtree_scale2_6p.Rda", verbose = T) #This will give calibration_measure_imp data frame, which contains the quantitative measures of calibration performance.
load("modifiedSP_cstat_lasso_regular_baggedtree_scale2_6p.Rda", verbose = T) #This will give c_stat_imp data frame, which contains the C-statistic.
load("modifiedSP_simpen_lasso_regular_baggedtree_scale2_6p.Rda", verbose = T) #This will give simpen data frame, which contains the whole data frame used for prediction, including the predicted probability from the model.

###O/E ratio###
O_E = simpen %>% 
  dplyr::group_by(cluster_code) %>% 
  dplyr::summarize(O = sum(as.numeric(as.character(Died))), E = sum(.pred_1)) %>% mutate(O_E = O/E) %>%
  left_join(simpen %>%
              mutate(mult = .pred_1*(1-.pred_1)) %>%
              select(mult, cluster_code) %>%
              dplyr::group_by(cluster_code) %>%
              dplyr::summarize(mult = sum(mult)),
            by = c("cluster_code" = "cluster_code"))

O_E = O_E %>%
  mutate(sd = 1/E * sqrt(mult))

forest(metagen(TE = O_E,
               seTE = sd,
               studlab = cluster_code,
               data =  O_E,
               prediction = T, 
               method.random.ci = "HK",
               method.predict = "HK"),
       xlim = c(0.5, 1.5))
grid.text("Lasso logistic regression O/E ratio", .5, 0.8, gp=gpar(cex=1.5))

#Calibration slope
forest(metagen(TE = cal_slope,
               seTE = se_slope,
               studlab = cluster_code,
               data = calibration_measure_imp,
               null = 1,
               prediction = T,
               method.random.ci  = "HK",
               method.predict = "HK", 
               method.tau = "REML"),
       #sortvar = TE,
       ref = 1,
       xlim = c(0.4, 1.6))
grid.text("Lasso logistic regression calibration slope", .5, .8, gp=gpar(cex=2))

###Calibration intercept###
forest(metagen(TE = cal_int,
               seTE = se_int,
               studlab = cluster_code,
               data =  calibration_measure_imp,
               null = 0,
               method.random.ci  = "HK",
               method.predict = "HK",
               method.tau = "REML",
               prediction = T),
       ref = 0)
grid.text("Lasso logistic regression calibration intercept", .5, .8, gp=gpar(cex=2))


###C-statistic###
forest(metagen(TE = roc_auc,
               seTE = se_roc_auc,
               studlab = cluster_code,
               data =  c_stat_imp,
               method.random.ci = "HK",
               method.predict = "HK",
               prediction = T),
       xlim = c(0.6, 1))
grid.text("Lasso logistic regression C-statistics", .5, 0.8, gp=gpar(cex=1.5))


###Brier Score###
simpen %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)


###Smoothed calibration plot###
pmcalibration(y = (simpen %>% mutate(Died = as.numeric(as.character(Died))))$Died, p = simpen$.pred_1, smooth = "loess", ci = "boot", n = 1000) %>% plot()


###Distribution of predicted probability###
ggplot(simpen, aes(x = .pred_1)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100), 
                 bins = 20, fill = "skyblue", color = "black") +
  labs(x = "Predicted Probability", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() 

###Decision curve analysis for all the models in IECV###
#Loading the results for all the models in IECV for decision curve analysis
load("modified_simpen_xgb_regular_baggedtree.Rda", verbose = T) #IECV results for sXGB
simpen_xgb = simpen

load("modifiedSP_simpen_lasso_regular_baggedtree_scale2.Rda", verbose = T) #IECV results for sLR
simpen_lasso = simpen

load("simpen_DT_regular_noimp.Rda", verbose = T) #IECV results for DT
simpen_dt = simpen

simpen_comb = simpen_xgb %>% #Combining them into one dataset
  mutate(.pred_1_lasso = simpen_lasso$.pred_1,
         .pred_1_DT = simpen_dt$.pred_1) %>%
  rename("sXGB" = ".pred_1",
         "sLR" = ".pred_1_lasso",
         "DT" = ".pred_1_DT")

#Using the emergency signs
simpen_comb = simpen_comb %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0)) 

#Decision curve analysis for all the models
dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
    data = simpen_comb %>%
      mutate(Died = factor(Died, levels = c("0", "1"))) %>%
      filter(age2 > 60),
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
  coord_cartesian(ylim = c(-0.01, 0.04)) +
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


###Decision curve analysis for SpO2 by itself###
final_child = final_child %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80&Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85&Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90&Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0)) 

dca(Died ~ `SpO2<80` + `SpO2<85` + `SpO2<90`, 
    data = final_child %>%
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
  scale_color_manual(values = c("SpO2<80" = "#00CD66", 
                                "SpO2<85" = "#FFFF00",
                                "SpO2<90" = "#CD853F",
                                "Treat All" = "#D02090",
                                "Treat None" = "#FF0000")) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "darkgrey")

##################PERFORMING SUBGROUP ANALYSES BASED on SEX##################

###Load all the results for the simplified lasso logistic regression (sLR); using simplified lasso logistic regression as example###
load("modifiedSP_cal_lasso_regular_baggedtree_scale2_6p.Rda", verbose = T) #This will give calibration_measure_imp data frame, which contains the quantitative measures of calibration performance.
load("modifiedSP_cstat_lasso_regular_baggedtree_scale2_6p.Rda", verbose = T) #This will give c_stat_imp data frame, which contains the C-statistic.
load("modifiedSP_simpen_lasso_regular_baggedtree_scale2_6p.Rda", verbose = T) #This will give simpen data frame, which contains the whole data frame used for prediction, including the predicted probability from the model.

#This loop would print out the C-statistic, calibration slope, calibration intercept, and O/E ratio for each subgroup.
#0 is for female; 1 is for male.
for (i in c("0", "1")) {
  print(i)
  
  dat = simpen %>%
    filter(Q12SEX == i) %>%
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
  print(coef(summary(slope_pc))[2,1])
  print(coef(summary(slope_pc))[2,1] - 1.96 * coef(summary(slope_pc))[2,2])
  print(coef(summary(slope_pc))[2,1] + 1.96 * coef(summary(slope_pc))[2,2])
  
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
  filter(Q12SEX == "0") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)

#Brier Class for male
simpen %>%
  filter(Q12SEX == "1") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)

#Loading the results for all the models for subgroup decision curve analysis
load("modified_simpen_xgb_regular_baggedtree.Rda", verbose = T) #IECV results for sXGB
simpen_xgb = simpen

load("modifiedSP_simpen_lasso_regular_baggedtree_scale2.Rda", verbose = T) #IECV results for sLR
simpen_lasso = simpen

load("simpen_DT_regular_noimp.Rda", verbose = T) #IECV results for DT
simpen_dt = simpen

simpen_comb = simpen_xgb %>%
  mutate(.pred_1_lasso = simpen_lasso$.pred_1,
         .pred_1_DT = simpen_dt$.pred_1) %>%
  rename("sXGB" = ".pred_1",
         "sLR" = ".pred_1_lasso",
         "DT" = ".pred_1_DT")

#Using the emergency signs
simpen_comb = simpen_comb %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_Coma` = if_else(Q52SP02P < 80 | Q45COMA2 == 1, 1, 0),
         `SpO2<85_OR_Coma` = if_else(Q52SP02P < 85 | Q45COMA2 == 1, 1, 0),
         `SpO2<90_OR_Coma` = if_else(Q52SP02P < 90 | Q45COMA2 == 1, 1, 0)) 

#Decision curve analysis for female
fem = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
          data = simpen_comb %>%
            mutate(Died = factor(Died, levels = c("0", "1"))) %>%
            filter(Q12SEX == "0"),
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
  coord_cartesian(ylim = c(-0.01, 0.04)) +
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

#Decision curve analysis for male
mal = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_Coma` + `SpO2<85_OR_Coma` + `SpO2<90_OR_Coma`, 
          data = simpen_comb %>%
            mutate(Died = factor(Died, levels = c("0", "1"))) %>%
            filter(Q12SEX == "1"),
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
  coord_cartesian(ylim = c(-0.01, 0.04)) +
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

