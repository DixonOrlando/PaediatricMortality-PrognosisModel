//////////External Validation Sample Size Calculation for Lasso Logistic Regression//////////


clear all
sknor 1000000 1111 -3.85 1.18 1.5 6 ///modified parameter value to use the available option for the function.
hist skewnormal
gen P = exp(skewnormal)/(1+exp(skewnormal)) 
gen outcome = rbinomial(1,P)
summ skewnormal, detail
summ outcome



* STEP (i): O/E calculation
* target confidence interval width of 1 for O/E; assuming O/E is 1 then this
* corresponds to SE(lnOE) of 0.245
clear all
* inputs
local selnoe = 0.245
local outcome_prop = 0.04
* calculation
local sampsize_OE = (1- `outcome_prop')/(`outcome_prop'*`selnoe'*`selnoe')
disp "Required sample size (events) for O/E = " `sampsize_OE' " (" `sampsize_OE'*`outcome_prop' ")" 

* STEP (ii): calibration slope calculation
clear all
sknor 1000000 1111 -3.85 1.18 1.5 6
gen LP = skewnormal
* input assumed parameters of calibration model
* below assumes `weak' level calibration is obtained, i.e. intercept of 0 and slope of 1
local beta0 = 0
local beta1 = 1
* calculate elements of I matrix
gen Borenstein_00 = exp(`beta0' + (`beta1'*LP))/((1+ exp(`beta0' + (`beta1'*LP)))^2)
gen Borenstein_01 = LP*exp(`beta0' + (`beta1'*LP))/((1+ exp(`beta0' + (`beta1'*LP)))^2)
gen Borenstein_11 = LP*LP*exp(`beta0' + (`beta1'*LP))/((1+ exp(`beta0' + (`beta1'*LP)))^2)
qui summ Borenstein_00
local I_00 = r(mean)
disp "I_00 =" r(mean)
qui summ Borenstein_01
local I_01 = r(mean)
disp "I_01 =" r(mean)
qui summ Borenstein_11
local I_11 = r(mean)
disp "I_11 =" r(mean)
* input desired SE: target a CI width of 0.2, which corresponds to a SE of 0.051
local seslope = 0.051
* alternatively: local selnoe = CIwidth/(2*1.96)
* input outcome event proportion
local outcome_prop = 0.04
* calculate sample size
local sampsize_slope = (`I_00'/(`seslope'*`seslope'*((`I_00'*`I_11')-(`I_01'*`I_01'))))
disp "Required sample size (events) for calibration slope = " `sampsize_slope' " (" `sampsize_slope'*`outcome_prop' ")" 

* STEP (iii): C-statistic calculation
clear all
local outcome_prop = 0.04
local Cstat = 0.85
* No closed form solution so need an iterative or deductive approach
* First calculate the SE for a range of N
set obs 10000000
gen size = _n
gen seCstatsq = `Cstat'*(1-`Cstat')*(1+(((size/2)-1)*((1-`Cstat')/(2-`Cstat')))+((((size/2)-1)*`Cstat')/(1+`Cstat')))/(size*size*`outcome_prop'*(1-`outcome_prop'))
gen seCstat = sqrt(seCstatsq)
gen CIwidth = 2*1.96*seCstat 
* Now identify the sample sizes that give a CIwidth no wider than the desired value
drop if CIwidth>0.1000
* Finally identify the minimum of all these sample sizes
qui summ size
local sampsize_Cstat = r(min)
disp "Required sample size (events) for C-statistic " `sampsize_Cstat' " (" `sampsize_Cstat'*`outcome_prop' ")"


* STEP (iv): Net benefit calculation
*5% threshold
* inputs
clear all
local outcome_prop = 0.04
local sens = 0.68
local spec = 0.86
local threshold = 0.05
local NB = (`sens'*`outcome_prop') - ((1-`spec')*(1-`outcome_prop')*(`threshold'/(1-`threshold')))
local sNB = `NB'/`outcome_prop'
* calculate NB and sNB
disp "net benefit =  `NB' 
disp "standardised net benefit = " `sNB'
local w = ((1-`outcome_prop')/`outcome_prop')*(`threshold'/(1-`threshold'))
* target CI width for sNB of 0.2, which corresponds to SE of 0.051
* input SE
local sesNB = 0.051
* calculate sample size
local sampsize_sNB = (1/(`sesNB'^2))*(((`sens'*(1-`sens'))/`outcome_prop')+(`w'*`w'*`spec'*(1-`spec')/(1-`outcome_prop'))+(`w'*`w'*(1-`spec')*(1-`spec')/(`outcome_prop'*(1-`outcome_prop'))))
disp "Required sample size (events) for standardised net benefit = " `sampsize_sNB' " (" `sampsize_sNB'*`outcome_prop' ")"

*20% threshold
* inputs
clear all
local outcome_prop = 0.04
local sens = 0.32
local spec = 0.97
local threshold = 0.20
local NB = (`sens'*`outcome_prop') - ((1-`spec')*(1-`outcome_prop')*(`threshold'/(1-`threshold')))
local sNB = `NB'/`outcome_prop'
* calculate NB and sNB
disp "net benefit =  `NB' 
disp "standardised net benefit = " `sNB'
local w = ((1-`outcome_prop')/`outcome_prop')*(`threshold'/(1-`threshold'))
* target CI width for sNB of 0.2, which corresponds to SE of 0.051
* input SE
local sesNB = 0.051
* calculate sample size
local sampsize_sNB = (1/(`sesNB'^2))*(((`sens'*(1-`sens'))/`outcome_prop')+(`w'*`w'*`spec'*(1-`spec')/(1-`outcome_prop'))+(`w'*`w'*(1-`spec')*(1-`spec')/(`outcome_prop'*(1-`outcome_prop'))))
disp "Required sample size (events) for standardised net benefit = " `sampsize_sNB' " (" `sampsize_sNB'*`outcome_prop' ")"

