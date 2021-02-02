# Chilko smolt life history over time comparisons 
# 7-Dec-2020

###################################################################################################################################################

setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(lubridate)
library(egg)
library(xlsx)
library(openxlsx)

options(scipen = 9999)

length.raw <- read.xlsx("chilko_smoltlength_timeseries.xlsx", sheet="data", detectDates=T)
fry.raw <- read.xlsx("FryDB(Verified2019BB).xlsx", sheet="FryData")
rec.dev.raw <- read.csv("Chilko_LarkinModel_Deviates_forKatie.csv")
esc.raw <- read.xlsx("DFO Sockeye Escapement All Years (June 2020).xlsx", sheet="SKAll")

###################################################################################################################################################

#                                                           CLEANING/CALCULATIONS/MAKING DFs

##############
# SMOLT DATA #
##############

length.data <- length.raw %>% 
  rename(smolt_year=year) %>%
  print()

#-------- Calculate daily total migration 
total <- length.data %>% 
  group_by(smolt_year) %>% 
  summarize(age1_total=sum(n_age1, na.rm=T), age2_total=sum(n_age2, na.rm=T)) %>%
  mutate(smolt_total=age1_total+age2_total) %>%
  print()

#-------- Calculate yearly weighted and unweighted average length
#----PRE-2014 (no smooth) 
# AGE 1 
age1_lengths_pre14 <- length.data %>% 
  filter(!is.na(length_age1_dayavg)) %>% 
  group_by(smolt_year) %>%
  summarize(length_weightavg_age1 = sum(weight_fact_age1)/sum(n_age1), 
    length_unweightavg_age1 = mean(length_age1_dayavg), length_unweightse_age1=sd(length_age1_dayavg)/sqrt(length(length_age1_dayavg))) %>%
  print()

# AGE 2 
age2_lengths_pre14 <- length.data %>% 
  filter(!is.na(length_age2_dayavg)) %>% 
  group_by(smolt_year) %>%
  summarize(length_weightavg_age2 = sum(weight_fact_age2)/sum(n_age2), 
    length_unweightavg_age2 = mean(length_age2_dayavg), length_unweightse_age2=sd(length_age2_dayavg)/sqrt(length(length_age2_dayavg))) %>%
  print()

#----POST-2014 (no smooth) 
# AGE 1
age1_lengths_post14 <- length.data %>% 
  filter(!is.na(length_age1_smdayavg)) %>% 
  group_by(smolt_year) %>%
  summarize(length_weightavg_age1 = sum(weight_fact_age1)/sum(n_age1), 
    length_unweightavg_age1 = mean(length_age1_smdayavg), length_unweightse_age1=sd(length_age1_smdayavg)/sqrt(length(length_age1_smdayavg))) %>%
  print()

# AGE 2 
age2_lengths_post14 <- length.data %>% 
  filter(!is.na(length_age2_smdayavg)) %>% 
  group_by(smolt_year) %>%
  summarize(length_weightavg_age2 = sum(weight_fact_age2, na.rm=T)/sum(n_age2), 
    length_unweightavg_age2 = mean(length_age2_smdayavg), length_unweightse_age2=sd(length_age2_smdayavg)/sqrt(length(length_age2_smdayavg))) %>%
  print()

#-------- Joins
age2s <- rbind(age2_lengths_pre14, age2_lengths_post14)
age1s <- rbind(age1_lengths_pre14, age1_lengths_post14)
smolt.df <- full_join(age2s, age1s, by="smolt_year")
smolt.df <- left_join(smolt.df, total, by="smolt_year")

############################################
# ADD IN HISTORICAL ANNUAL MIGRATION YEARS #
############################################



#-------- Export for RMarkdown
write.csv(smolt.df, "chilko_smolt_df.csv", row.names=F)


############
# FRY DATA #
############

fry.df <- fry.raw %>% 
  rename(date=Date,
    system=Stream,
    sample=`Samp#`,
    length_mm=`Length.(mm)`,
    ww_g=`Wet.Wt.(g)`,
    dw_g=`Dry.Wt.(g)`,
    year=YEAR,
    ufid=ID) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  filter(system=="Chilko") %>%
  group_by(year) %>%
  summarize(fry_length_mean=mean(length_mm), fry_length_se=sd(length_mm)/sqrt(length(length_mm)), 
    fry_ww_mean=mean(ww_g), fry_ww_se=sd(ww_g)/sqrt(length(ww_g))) %>%
  rename(fry_year=year) %>%
  print()

#-------- Export for RMarkdown
write.csv(fry.df, "chilko_fry_df.csv", row.names=F)


###############################
# LARKIN RECRUITMENT DEVIATES #
###############################

rec.df <- rec.dev.raw %>% 
  rename(brood_year=BroodYear,
    r_obs=R_Obs,
    r_pred=R_pred,
    rec_dev=Dev) %>%
  print()

#-------- Export for RMarkdown
write.csv(rec.df, "chilko_rdev_df.csv", row.names=F)


#######################
# ESCAPEMENT/EFS DATA #
#######################

efs.df <- esc.raw %>% 
  rename(efs_year=Year,
    watershed_group=Watershed.Group.Name,
    stock=`Stock.Name(stream)`,
    timing_group=Timing.Group,
    cu=CU.Name,
    total=Total,
    adults=Adults,
    perc_spawn=`spawn%`,
    efs=eff_fem) %>%
  filter(cu%in%c("Chilko - Early Summer", "Chilko - Summer")) %>% 
  group_by(efs_year) %>% 
  summarize(efs = sum(efs)) %>%
  print()

#-------- Export for RMarkdown
write.csv(efs.df, "chilko_efs_df.csv", row.names=F)



###################################################################################################################################################

#                                                                 SUMMARY STATS

ages <- smolt.df %>%
  group_by(smolt_year) %>%
  summarize(prop_age1 = age1_total/(age1_total+age2_total)) %>%
  print()

# given that smolts are predominantly age-1, will proceed with just comparisons using age1 smolts for simplicity.

smolts_a1 <- smolt.df %>% 
  select(smolt_year, length_weightavg_age1, length_unweightavg_age1, length_unweightse_age1, age1_total, smolt_total) %>%
  print()

###################################################################################################################################################













