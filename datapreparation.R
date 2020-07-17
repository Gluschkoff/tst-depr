# required packages -------------------------------------------------------
library(nhanesA)
library(tidyverse)
library(survey)
library(summarytools)
library(jtools) # for summ
library(splines) # for the analysis
library(ggpubr) # for plots

# data download and merging -------------------------------------------------------
# Note! When combining TST data from different cycles, conversion is needed:	
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/TST_H.htm
# TST data
tst11 <- nhanes('TST_G')  
tst13 <- nhanes("TST_H") %>% select("SEQN", "LBXTST")
tst13$LBXTST <- 0.979*(tst13$LBXTST) - 0.178
tst15 <- nhanes("TST_I") %>% select("SEQN", "LBXTST") 
tst15$LBXTST <- 0.979*(tst15$LBXTST) - 0.178               
# demographic data
demo11 <- nhanes("DEMO_G") %>% select("SEQN" , "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA" ) 
demo13 <- nhanes("DEMO_H") %>% select("SEQN" , "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")          
demo15 <- nhanes("DEMO_I") %>% select("SEQN" , "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")
# BMI data
bmi11 <- nhanes("BMX_G") %>% select("SEQN", "BMXBMI")
bmi13 <- nhanes("BMX_H") %>% select("SEQN", "BMXBMI")
bmi15 <- nhanes("BMX_I") %>% select("SEQN", "BMXBMI")
#smoking 
smo11 <- nhanes("SMQ_G") %>% select("SEQN", "SMQ020", "SMQ040")
smo13 <- nhanes("SMQ_H") %>% select("SEQN", "SMQ020", "SMQ040")
smo15 <- nhanes("SMQ_I") %>% select("SEQN", "SMQ020", "SMQ040")
# alcohol use
alc11 <- nhanes("ALQ_G") %>% select("SEQN", "ALQ101", "ALQ110", "ALQ120Q", "ALQ130")
alc13 <- nhanes("ALQ_H") %>% select("SEQN", "ALQ101", "ALQ110", "ALQ120Q", "ALQ130")
alc15 <- nhanes("ALQ_I") %>% select("SEQN", "ALQ101", "ALQ110", "ALQ120Q", "ALQ130")
# physical activity
pa11 <- nhanes("PAQ_G") %>% select("SEQN", "PAQ665", "PAQ650")
pa13 <- nhanes("PAQ_H") %>% select("SEQN", "PAQ665", "PAQ650")
pa15 <- nhanes("PAQ_I") %>% select("SEQN", "PAQ665", "PAQ650")
# pregnancy 
pre11 <- nhanes("RHQ_G") %>% select("SEQN","RHD143")
pre13 <- nhanes("RHQ_H") %>% select("SEQN","RHD143")
pre15 <- nhanes("RHQ_I") %>% select("SEQN","RHD143")
#depression data
dep11 <- nhanes("DPQ_G") %>% na_if(7) %>% na_if(9)
dep13 <- nhanes("DPQ_H") %>% na_if(7) %>% na_if(9)  
dep15 <- nhanes("DPQ_I") %>% na_if(7) %>% na_if(9)
# merging datasets 2011, 2013, 2015
df11 <- tst11 %>% full_join(dep11, by="SEQN", all=T) %>% full_join(demo11, by="SEQN") %>% full_join(bmi11, by="SEQN") %>% full_join(smo11, by="SEQN") %>% full_join(alc11, by="SEQN") %>% full_join(pa11, by="SEQN") %>% full_join(pre11, by="SEQN")
df13 <- tst13 %>% full_join(dep13, by="SEQN", all=T) %>% full_join(demo13, by="SEQN") %>% full_join(bmi13, by="SEQN") %>% full_join(smo13, by="SEQN") %>% full_join(alc13, by="SEQN") %>% full_join(pa13, by="SEQN") %>% full_join(pre13, by="SEQN")
df15 <- tst15 %>% full_join(dep15, by="SEQN", all=T) %>% full_join(demo15, by="SEQN") %>% full_join(bmi15, by="SEQN") %>% full_join(smo15, by="SEQN") %>% full_join(alc15, by="SEQN") %>% full_join(pa15, by="SEQN") %>% full_join(pre15, by="SEQN")
dfall <- rbind(df11, df13, df15)

# filtering data, recoding and creating variables -------------------------------------------------------
dffull <- dfall %>%
  filter(!is.na(LBXTST)) %>% # drop if TST missing
  filter(RIDAGEYR >17) %>%  # age 18 or older
  filter((RIAGENDR==2 & LBXTST<71) | (RIAGENDR==1 & LBXTST<1101)) %>% # drop if TST values abnormally high
  filter_at(vars(DPQ010:DPQ100), all_vars(!is.na(.))) # drop if missing values in depressive symptoms
# recode binary dep symptoms and create dep sum score
dffull <- dffull %>% mutate(
  depd1=ifelse(DPQ010==0|DPQ010==1, 0, ifelse(is.na(DPQ010)==1, NA,1)),
  depd2=ifelse(DPQ020==0|DPQ020==1, 0, ifelse(is.na(DPQ020)==1, NA,1)),
  depd3=ifelse(DPQ030==0|DPQ030==1, 0, ifelse(is.na(DPQ030)==1, NA,1)),
  depd4=ifelse(DPQ040==0|DPQ040==1, 0, ifelse(is.na(DPQ040)==1, NA,1)),
  depd5=ifelse(DPQ050==0|DPQ050==1, 0, ifelse(is.na(DPQ050)==1, NA,1)),
  depd6=ifelse(DPQ060==0|DPQ060==1, 0, ifelse(is.na(DPQ060)==1, NA,1)),
  depd7=ifelse(DPQ070==0|DPQ070==1, 0, ifelse(is.na(DPQ070)==1, NA,1)),
  depd8=ifelse(DPQ080==0|DPQ080==1, 0, ifelse(is.na(DPQ080)==1, NA,1)),
  depd9=ifelse(DPQ090==0|DPQ090==1, 0, ifelse(is.na(DPQ090)==1, NA,1))) %>%
  mutate(depsum=DPQ010+DPQ020+DPQ030+DPQ040+DPQ050+DPQ060+DPQ070+DPQ080+DPQ090)
# smoking 
dffull <- dffull %>% mutate(smo=ifelse(SMQ020==1 & SMQ040==1, 1, ifelse(SMQ020==2 | SMQ040==2 | SMQ040==3, 0, NA)))
# alcohol use, recode missing values
dffull$ALQ130[dffull$ALQ130==999 | dffull$ALQ130==777] <- NA # Avg alcoholic drinks/day - past 12 mos
dffull$ALQ120Q[dffull$ALQ120Q==999 | dffull$ALQ120Q==777] <- NA # How often drink alcohol over past 12 mos
# heavy drinking, men
dffull$alcm <- NA  
dffull$alcm[dffull$ALQ130<5 & dffull$RIAGENDR==1] <- 0 
dffull$alcm[dffull$ALQ130>=5 & dffull$ALQ120Q>=1 & dffull$RIAGENDR==1] <- 1 
# heavy drinking, women
dffull$alcw <- NA  
dffull$alcw[dffull$ALQ130<4 & dffull$RIAGENDR==2] <- 0 
dffull$alcw[dffull$ALQ130>=4 & dffull$ALQ120Q>=1 & dffull$RIAGENDR==2] <- 1
# heavy drinking, combined
dffull$alc <- ifelse(dffull$RIAGENDR==1, dffull$alcm, dffull$alcw)
dffull$alc[dffull$ALQ101==2 | dffull$ALQ110==2 | dffull$ALQ120Q==0] <- 0 #alq101==2 means less than 12 drinks per year, alq110==2 less than 12 drinks in a lifetime
# physical activity
dffull$PAQ650[dffull$PAQ650==7 | dffull$PAQ650==9] <- NA # Vigorous recreational activities
dffull$PAQ650[dffull$PAQ650==2] <- 0 # no activity 
dffull$PAQ665[dffull$PAQ665==7 | dffull$PAQ665==9] <- NA # Moderate recreational activities
dffull$PAQ665[dffull$PAQ665==2] <- 0 # no moderate activity
dffull$pa <- NA
dffull$pa[dffull$PAQ665==0 & dffull$PAQ650==0] <- 0 #no moderate or vigorous
dffull$pa[dffull$PAQ665==1] <- 1 # no vigorous but at least moderate
dffull$pa[dffull$PAQ650==1] <- 2 #vigorous
# pregnancy 
dffull$preg <- 0
dffull$preg[dffull$RHD143==1] <- 1
# 6-year weight, see https://www.cdc.gov/nchs/tutorials/NHANES/SurveyDesign/Weighting/Task2.htm
dffull$weight = 1/3*dffull$WTMEC2YR
#renaming variables
names(dffull) <- c("SEQN", "tst", "dep1", "dep2","dep3","dep4","dep5","dep6","dep7","dep8", "dep9",
"dep10", "SDDSRVYR", "sex", "age", "exam_weight", "psu", "strata", "bmi", "SMQ020",  "SMQ040" , "ALQ101" ,  "ALQ110",   "ALQ120Q" ,"ALQ130"  , "PAQ665" ,  "PAQ650" ,  "RHD143", "depd1", "depd2",    "depd3",   "depd4",  "depd5",  "depd6", "depd7", "depd8", "depd9", "depsum", "smo", "alcm", "alcw", "alc", "pa", "preg", "weight")
#categorize variables 
categ <- c("dep1", "dep2","dep3","dep4","dep5","dep6","dep7","dep8", "dep9",  "dep10", "sex", "depd1","depd2","depd3","depd4","depd5","depd6","depd7","depd8","depd9", "smo", "alc", "pa","preg")
dffull[,categ] <- lapply(dffull[,categ], factor)   
dffull$bmi <- as.numeric(dffull$bmi)
# drop if missingness in covariates
dffull <-dplyr::filter(dffull,!is.na(age) & !is.na(bmi) & !is.na(smo) & !is.na(pa) & !is.na(alc))
# create new TST variables ((1)low/normal and (2)log)
dffull <- dffull %>% mutate(tstdikm=case_when(tst<300 & sex==1 ~ 1, tst>=300 & sex==1 ~0)) %>%    
                     mutate(tstdikw=case_when(tst<15 & sex==2 ~ 1, tst>=15 & sex==2 ~0)) %>% mutate(logtst=log(tst))
# svy design
des <- svydesign(id=~psu, strata=~strata, weights=~weight, nest=TRUE,data=dffull)
desmen <- subset(des, sex==1) 
deswomen <- subset(des, sex==2) 
