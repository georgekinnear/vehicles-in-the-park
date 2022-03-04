#############################################################################
# R script for processing CJ decision data for Vehicles in Park study
# Nov 2021
#############################################################################

  #rm(list = ls())

#############################################################################
# sirt is the package that contains the btm commands
# also need dplyr for transforming data
#############################################################################
  library(sirt)
  require(dplyr)
  require(cocor)
  library("ggpubr")
  source("sirt_functions.R")

#############################################################################
# get Tobia's data
#############################################################################
  tobia <- read.csv('Tobiap33fig5.csv', header = T, stringsAsFactors = F)
  

#############################################################################
# get the decision data and remove attention checks and 
# the judges who failed them
#############################################################################
  decisions <- read.csv('judgements.csv', header=T, stringsAsFactors=F)
  decisions <- decisions[decisions$left!=0,]
  decisions <- decisions[decisions$right!=0,]
  decisions <- decisions[decisions$judge_id!=59,] 
  decisions <- decisions[decisions$judge_id!=110,]  

#############################################################################  
# NUISANCE PAIRS
#############################################################################
# fit to BT model and get scale values and SSR and interrater reliability
  nuisance_pairs_mod1 <- btm_with_judges(decisions[decisions$study=="nuisance_pairs",])
  nuisance_pairs_SSR <- nuisance_pairs_mod1$mle.rel 
  nuisance_pairs_interrater <- interrater_rel(decisions[decisions$study=="nuisance_pairs",])
# calculate misfit
  nuisance_pairs_judgefit <- judge_misfit(nuisance_pairs_mod1$fit_judges)
  nuisance_pairs_scriptfit <- script_misfit(nuisance_pairs_mod1$residuals)

  
  #############################################################################  
  # VEHICLE PAIRS
  #############################################################################
  # fit to BT model and get scale values and SSR and interrater reliability
  vehicle_pairs_mod1 <- btm_with_judges(decisions[decisions$study=="vehicle_pairs",])
  vehicle_pairs_SSR <- vehicle_pairs_mod1$mle.rel 
  vehicle_pairs_interrater <- interrater_rel(decisions[decisions$study=="vehicle_pairs",])
  # calculate misfit
  vehicle_pairs_judgefit <- judge_misfit(vehicle_pairs_mod1$fit_judges)
  vehicle_pairs_scriptfit <- script_misfit(vehicle_pairs_mod1$residuals)
  
  
  #############################################################################  
  # VIOLATION PAIRS
  #############################################################################
  # fit to BT model and get scale values and SSR and interrater reliability
  violation_pairs_mod1 <- btm_with_judges(decisions[decisions$study=="violation_pairs",])
  violation_pairs_SSR <- violation_pairs_mod1$mle.rel 
  violation_pairs_interrater <- interrater_rel(decisions[decisions$study=="violation_pairs",])
  # calculate misfit
  violation_pairs_judgefit <- judge_misfit(violation_pairs_mod1$fit_judges)
  violation_pairs_scriptfit <- script_misfit(violation_pairs_mod1$residuals)
  
  #############################################################################  
  # normality checks
  #############################################################################  
   tobia_percent <- as.numeric(tobia$percentage[1:25])
  # ggdensity(tobia_percent, 
  #           main = "Density",
  #           xlab = "%")
  # ggqqplot(tobia_percent)
  # shapiro.test(tobia_percent) #tobia not normal so spearman justified
  #all CJ data is normal
  

  #############################################################################  
  # H2: pearson correlations
  #############################################################################
  h2 <- cbind(nuisance = nuisance_pairs_mod1$effects$theta, vehicle  = vehicle_pairs_mod1$effects$theta, violation = violation_pairs_mod1$effects$theta)
  round(cor(h2),3)
  
  #test the difference between correlations
  cocor(~nuisance + vehicle | nuisance + violation, data = h2, test = "hittner2003")
  #sig different p-value = 0.0010
  cocor(~vehicle + nuisance | vehicle + violation, data = h2, test = "hittner2003")
  #sig different p-value = 0.0046
  cocor(~violation + vehicle | violation + nuisance, data = h2, test = "hittner2003")
  #no sig difference p-value = 0.4334
  #############################################################################  
  # H2: spearman correlations
  #############################################################################
  h3 <- cbind(tobia = tobia_percent, h2)
  round(cor(h3, method = "spearman"),3)
  
  #test the difference between correlations
  cocor(~tobia + vehicle | tobia + violation, data = h3, test = "hittner2003")
  #sig different p-value = 0.0389
  cocor(~tobia + nuisance | tobia + violation, data = h3, test = "hittner2003")
  #no sig different p-value = 0.3206
  cocor(~tobia + vehicle | tobia + nuisance, data = h3, test = "hittner2003")
  #no sig difference p-value = 0.0369
  