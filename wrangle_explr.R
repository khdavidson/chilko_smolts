# Chilko smolt life history over time comparisons 
# 7-Dec-2020

#######################################################################################################################################################

setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(lubridate)
library(xlsx)
library(openxlsx)

options(scipen = 9999)

data.raw <- read.xlsx("chilko_smoltlength_timeseries.xlsx", sheet="data", detectDates=T)

#######################################################################################################################################################

#                                                                  CLEANING/CALCULATING

# calculate yearly weighted and unweighted average length
# AGE 1 (pre-2014 when it wasn't smoothed)
age1_lengths_pre14 <- data.raw %>% 
  filter(!is.na(length_age1_dayavg)) %>% 
  group_by(year) %>%
  summarize(length_weightavg_age1 = sum(weight_fact_age1)/sum(n_age1), 
    length_unweightavg_age1 = mean(length_age1_dayavg), length_unweightse_age1=sd(length_age1_dayavg)/sqrt(length(length_age1_dayavg))) %>%
  print()

# AGE 2 (pre-2014 when it wasn't smoothed)
age2_lengths_pre14 <- data.raw %>% 
  filter(!is.na(length_age2_dayavg)) %>% 
  group_by(year) %>%
  summarize(length_weightavg_age2 = sum(weight_fact_age2)/sum(n_age2), 
    length_unweightavg_age2 = mean(length_age2_dayavg), length_unweightse_age2=sd(length_age2_dayavg)/sqrt(length(length_age2_dayavg))) %>%
  print()

#---

# AGE 1 (post-2014 when it was smoothed)
age1_lengths_post14 <- data.raw %>% 
  filter(!is.na(length_age1_smdayavg)) %>% 
  group_by(year) %>%
  summarize(length_weightavg_age1 = sum(weight_fact_age1)/sum(n_age1), 
    length_unweightavg_age1 = mean(length_age1_smdayavg), length_unweightse_age1=sd(length_age1_smdayavg)/sqrt(length(length_age1_smdayavg))) %>%
  print()

# AGE 2 (post-2014 when it was smoothed)
age2_lengths_post14 <- data.raw %>% 
  filter(!is.na(length_age2_smdayavg)) %>% 
  group_by(year) %>%
  summarize(length_weightavg_age2 = sum(weight_fact_age2, na.rm=T)/sum(n_age2), 
    length_unweightavg_age2 = mean(length_age2_smdayavg), length_unweightse_age2=sd(length_age2_smdayavg)/sqrt(length(length_age2_smdayavg))) %>%
  print()


########
# JOIN #
########

age2s <- rbind(age2_lengths_pre14, age2_lengths_post14)
age1s <- rbind(age1_lengths_pre14, age1_lengths_post14)

data <- full_join(age2s, age1s, by="year")

write.csv(data, "chilko_lengths_weightunweight_export.csv", raw.names=F)

#######################################################################################################################################################

#                                                              SUMMARY/EXPLORATION

# Plot lengths over time 
ggplot() +
  geom_errorbar(data=data, aes(x=year, ymin=length_unweightavg_age2-length_unweightse_age2, ymax=length_unweightavg_age2+length_unweightse_age2), width=0, size=1, colour="gray60") +
  geom_point(data=data, aes(x=year, y=length_unweightavg_age2), shape=21, colour="gray60", fill="gray80", size=4, stroke=1.5) +
  geom_point(data=data, aes(x=year, y=length_weightavg_age2), shape=21, colour="black", fill="gray40", size=4, stroke=1.5, alpha=0.7) +
  
  geom_errorbar(data=data, aes(x=year, ymin=length_unweightavg_age1-length_unweightse_age1, ymax=length_unweightavg_age1+length_unweightse_age1), width=0, size=1, colour="gray60") +
  geom_point(data=data, aes(x=year, y=length_unweightavg_age1), shape=23, colour="gray60", fill="gray80", size=4, stroke=1.5) +
  geom_point(data=data, aes(x=year, y=length_weightavg_age1), shape=23, colour="black", fill="gray40", size=4, stroke=1.5, alpha=0.7) +
  scale_x_continuous(breaks=seq(1986,2020, by=3)) +
  labs(x="", y="Fork length (mm)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=12),
    axis.title = element_text(colour="black", size=15, face="bold"))








