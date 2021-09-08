# growth curves 
# mar 2021 

library(tidyverse)
library(readxl)
library(FSA)        # for vb() models
library(FSAdata)    # for practice data
library(car)        # for Boot()
library(nlstools)
library(data.table)

setwd("~/ANALYSIS/data")

vb.dat <- read.csv("chilko_smolt_vb_database.csv")


###########################################################################################################################################

                                                                # FUNCTIONS


# Function to filter data by year and age ranges
vb.year.fx <- function(vb_cohort_filter, vb_age_start, vb_age_end) {
  vb.dat%>% 
    filter(vb_year0 == !! vb_cohort_filter, vb_age >= !! vb_age_start & vb_age <= !! vb_age_end, !is.na(vb_l))
}
View(vb.year.fx(1997, 0.5, 6))


# Function to plot specific year   
vb.plot.fx <- function(cohort_year) {
  ggplot() +
    geom_line(data=vb.dat%>%filter(vb_year0 == !! cohort_year)%>%group_by(vb_age)%>%summarize(med_l=median(vb_l, na.rm=T)), aes(x=vb_age, y=med_l), size=1) +
    geom_point(data=vb.dat%>%filter(vb_year0 == !! cohort_year), aes(x=vb_age, y=vb_l, fill=sex), size=3.5, alpha=0.5, shape=21, colour="black") +
    #scale_y_continuous(limits=c(0,600)) +
    scale_x_continuous(limits=c(0,6)) +
    labs(x="Age", y="Length (mm)") +
    theme_bw() +
    theme(axis.text=element_text(size=14, colour="black"),
          axis.title=element_text(size=16, face="bold"),
          legend.position = c(0.8,0.2))
}
vb.plot.fx(1997)


###################################################   DO   NOT  RUN   BELOW   #############################################################


smolt.lf.raw <- read.csv("chilko_smolt_database_1986-2019_mar2021_LF.csv")
smolt.w.raw <- read_excel("chilko_smolt_database_1986-2019_mar2021.xlsx", sheet="chilkoSRfile", detectDates=T)
fry.raw <- read_excel("FryDB(Verified2019BB).xlsx", sheet="FryData")
psc.raw <- read.csv("PSC_master.csv")
psc.chilko <- read.csv("PSC_chilko_modelled_mar2021.csv")

###################################################################################################################################################

#                                                            CLEAN 

#---------- SMOLT LF DATA
# sometimes entered as 0s and sometimes as NAs, turn all NAs to 0 for consistency when expanding frequency table
smolt.lf <- smolt.lf.raw %>% 
  select(date:freq) %>%
  mutate(freq=ifelse(is.na(freq), 0, freq)) %>%
  mutate(date=lubridate::dmy(date)) %>%
  mutate(smoltprogram_year=lubridate::year(date)) %>%
  mutate(yday=lubridate::yday(date)) %>%
  mutate(BY=ifelse(age==1, smoltprogram_year-2, ifelse(age==2, smoltprogram_year-3, NA))) %>%
  mutate(year0 = ifelse(age==1, smoltprogram_year, ifelse(age==2, smoltprogram_year-1, NA))) %>%
  print()

# Expand LF data table 
smolt.lf <- smolt.lf[rep(1:nrow(smolt.lf), smolt.lf[["freq"]]), ]



#---------- SMOLT WEIGHTED LENGTH DATA
smolt.w <- smolt.w.raw %>% 
  rename(smoltprogram_year=year) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  pivot_longer(cols=c("weight_mean_length_1yo", "weight_mean_length_2yo"), names_to="age") %>% 
  mutate(age = ifelse(age=="weight_mean_length_1yo", 1, 2)) %>% 
  mutate(BY=ifelse(age==1, smoltprogram_year-2, ifelse(age==2, smoltprogram_year-3, NA))) %>%
  mutate(year0 = ifelse(age==1, smoltprogram_year, ifelse(age==2, smoltprogram_year-1, NA))) %>%
  rename(wlength_mm = value) %>%
  print()



#---------- FRY DATA
fry <- fry.raw %>% 
  rename(date=Date,
    system=Stream,
    sample=`Samp#`,
    length_mm=`Length (mm)`,
    ww_g=`Wet Wt (g)`,
    dw_g=`Dry Wt (g)`,
    year=YEAR,
    ufid=ID) %>%
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  filter(system=="Chilko") %>%
  rename(fryprogram_year=year) %>%
  mutate(year0 = fryprogram_year+1) %>%
  mutate(age = 0.5) %>%
  print()


#---------- PSC DATA  **** Does not need to be run now that it has been exported as a cleaned, modelled Chilko data set (line 172)
psc <- psc.raw %>% 
  filter(Stream_Shore%in%c("Chilko River", "Chilko Lake - South End", "Chilko Lake - North End")) %>%
  select(CollYear, Watershed.Group, Stream_Shore, ConditionCode, FinalAge, TotalAge, FreshwaterAge, POF:Sex) %>%
  rename(collection_year=CollYear,
    watershed_group=Watershed.Group,
    system=Stream_Shore,
    sample_condition=ConditionCode,
    total_age=FinalAge,
    adult_age=TotalAge,
    fwater_age=FreshwaterAge,
    pof=POF,
    poh=POH,
    std=STD,
    fork_sg=Fork,
    sex=Sex) %>%
  mutate(pof = as.numeric(ifelse(pof=="#N/A", NA, ifelse(pof=="", NA, pof)))) %>%
  mutate(poh = as.numeric(ifelse(poh=="#N/A", NA, ifelse(poh=="", NA, ifelse(poh=="0", NA, poh))))) %>%
  mutate(std = as.numeric(ifelse(std=="#N/A", NA, ifelse(std=="", NA, ifelse(std=="0", NA, std))))) %>%
  mutate(fork_sg = as.numeric(ifelse(fork_sg=="#N/A", NA, ifelse(fork_sg=="", NA, ifelse(fork_sg=="0", NA, fork_sg))))) %>%
  mutate(total_age = as.numeric(ifelse(total_age=="N/A", NA, total_age))) %>%
  mutate(adult_age = as.numeric(ifelse(adult_age=="N/A", NA, adult_age))) %>%
  mutate(fwater_age = as.numeric(ifelse(fwater_age=="N/A", NA, fwater_age))) %>%
  mutate(sex = ifelse(sex=="", "Unknown", sex)) %>%
  mutate(fork = NA) %>%
  mutate_at("sex", as.factor) %>%
  filter(adult_age %in% c(3:6), !is.na(pof)|!is.na(poh)|!is.na(std)|!is.na(fork_sg)) %>%   # removes cases where there is no length data at all
  mutate(year0 = ifelse(adult_age==3, collection_year-2, 
    ifelse(adult_age==4, collection_year-3, 
      ifelse(adult_age==5, collection_year-4, 
        ifelse(adult_age==6, collection_year-5, NA))))) %>% 
  print()


###################################################################################################################################################

#                                                            PSC LENGTH DATA 
#                  **** This section does not need to be re-run now that it has been exported in line 172 *****

# Calculate missing POH lengths based on linear regression with observed STD or observed Fork (if STD unavailable) and then convert to FL 
## FL is the end goal because smolts are measured in FL 


# Examine:
ggplot(psc, aes(x=poh, y=std)) +
  geom_point() +
  facet_grid(adult_age ~ sex)

# POF - POH regression 
ggplot(psc, aes(x=std, y=fork)) +
  geom_point() +
  facet_grid(adult_age ~ sex)

# Approach: 
# 1. Fill in is.na(poh) with known std's
# 2. Fill in is.na(poh) with known forks's 
# use other lengths to fill in anything else? 


#--------- 1. POH - STD regression 
# Linear model:
pohstd.lm <- lm(poh ~ std, data=psc)
r.pohstd.lm <- resid(pohstd.lm)
hist(r.pohstd.lm)
plot(r.pohstd.lm)
qqnorm(r.pohstd.lm)
qqline(r.pohstd.lm)
plot(pohstd.lm)
# rsid vs fit: obs 22845, 21392, 22640 are outliers 
# leverage points - 22845 and 21392 within cooks d=0.5, 22640 past cooks d=1
summary(pohstd.lm)

# Extract global linear equation: 
pohstd.lm.eqn <- pohstd.lm$coefficients[2]*psc$std + pohstd.lm$coefficients[1]

# Model missing STD lengths: 
psc.fill <- psc %>% 
  mutate(poh_source = ifelse(is.na(poh) & !is.na(std), "model_std", "obs_poh")) %>%
  mutate(poh = ifelse(is.na(poh), pohstd.lm.eqn, poh)) %>%
  print()


#--------- 2. POH - Fork regression 
# Linear model:
pohfk.lm <- lm(poh ~ fork, data=psc)
r.pohfk.lm <- resid(pohfk.lm)
hist(r.pohfk.lm)
plot(r.pohfk.lm)
qqnorm(r.pohfk.lm)
qqline(r.pohfk.lm)
plot(pohfk.lm)
# rsid vs fit: obs 15663, 15703, 15657 are outliers 
# leverage points - all within cooks d=0.5 
summary(pohfk.lm)

# Extract global linear equation: 
pohfk.lm.eqn <- pohfk.lm$coefficients[2]*psc$fork + pohfk.lm$coefficients[1]

# Model missing STD lengths: 
psc.fill <- psc.fill %>% 
  mutate(poh_source = ifelse(is.na(poh) & !is.na(fork), "model_fork", poh_source)) %>%
  mutate(poh = ifelse(is.na(poh), pohfk.lm.eqn, poh)) %>%
  print()


#--------- 3. STD - Fork regression 
# Linear model to infill missing STDs for missing POHs calculated from 'pohfk.lm.eqn' above (mostly just for plotting)
stdfk.lm <- lm(std ~ fork, data=psc)
r.stdfk.lm <- resid(stdfk.lm)
hist(r.stdfk.lm)
plot(r.stdfk.lm)   # meh
qqnorm(r.stdfk.lm)
qqline(r.stdfk.lm)
plot(stdfk.lm)
# rsid vs fit: obs 15589, 1135, 982 are outliers 
# leverage points - all within cooks d=0.5 
summary(stdfk.lm)

# Extract global linear equation: 
stdfk.lm.eqn <- stdfk.lm$coefficients[2]*psc$fork + stdfk.lm$coefficients[1]

# Model missing STD lengths: 
psc.fill <- psc.fill %>% 
  mutate(std_source = ifelse(is.na(std) & !is.na(fork), "model_fork", "obs_std")) %>%
  mutate(std = ifelse(is.na(std), stdfk.lm.eqn, std)) %>%
  print()


#--------- CONFIRM MODELLED LENGTHS:
#psc.fill$poh_source <- factor(psc.fill$poh_source, levels=c("model_fork", "model_std", "obs"), ordered=T)
ggplot(psc.fill%>%arrange(desc(poh_source)), aes(x=poh, y=std, colour=interaction(poh_source, std_source), fill=interaction(poh_source, std_source))) +
  geom_point(shape=21, size=3, alpha=0.1)


#--------- EXPORT FOR FASTER CALL NEXT TIME:
#write.csv(psc.fill, "PSC_chilko_modelled_mar2021.csv")




###################################################################################################################################################


#                                                       JOIN DATAFRAMES 

# Combine smolt and adult data frames for ease of plotting etc. 
# Data:
names(smolt.lf)       # (may be replaced later - e.g., by smolt.w)
names(psc)
names(fry)


#--------- First clean up PSC a bit better so it can match smolt.lf and vice-versa
psc.join <- psc %>%
  select(collection_year, adult_age, poh, sex,  year0) %>%
  rename(vb_age=adult_age, vb_year0=year0, vb_l=poh) %>% 
  print()

smolt.lf.join <- smolt.lf %>% 
  select(length_mm, age, smoltprogram_year, BY, year0) %>% 
  rename(vb_age=age, vb_year0=year0, vb_l=length_mm) %>%
  print()

fry.join <- fry %>% 
  select(length_mm, fryprogram_year, year0, age) %>% 
  rename(vb_age=age, vb_year0=year0, vb_l=length_mm) %>%
  print()


# JOIN
vb.join <- full_join(smolt.lf.join, psc.join, by=c("vb_age", "vb_year0", "vb_l"))
vb.join <- full_join(vb.join, fry.join, by=c("vb_age", "vb_year0", "vb_l"))

vb.join <- vb.join %>% 
  filter(vb_year0 >= 1982)

# EXPORT
write.csv(vb.join, "chilko_smolt_vb_database.csv", row.names=F)

###########################################################################################################################################

#                                                               VISUALIZE/INVESTIGATE 

# All years facet plot
megaplot <- ggplot() +
  geom_line(data=vb.dat%>%group_by(vb_year0, vb_age)%>%summarize(med_l=median(vb_l, na.rm=T)), aes(x=vb_age, y=med_l, group=vb_year0), size=0.65) +
  geom_point(data=vb.dat, aes(x=vb_age, y=vb_l, fill=sex), size=3, alpha=0.5, shape=21, colour="black") +
  scale_x_continuous(limits=c(0,6), breaks=seq(0,6,by=1)) +
  labs(x="Age", y="Length (mm)") +
  theme_bw() +
  theme(axis.text=element_text(size=14, colour="black"),
    axis.title=element_text(size=16, face="bold"),
    legend.position = c(0.93,0.03),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill="white")) +
  facet_wrap(~vb_year0)

# Export plot: recent as of Aug 2021
# ggsave(plot=megaplot, filename="vbplots.pdf", width=11, height=8.5, units="in", path="~/Documents/ANALYSIS/figures")
  


###################################################################################################################################################

# Resources:
# https://www.r-bloggers.com/2019/12/von-bertalanffy-growth-plots-i/
# https://www.r-bloggers.com/2013/11/a-problem-fitting-the-von-bertalanffy-growth-model-with-nls/ 
# http://derekogle.com/fishR/examples/oldFishRVignettes/VonBertalanffy.pdf

# BEST SITE: http://derekogle.com/fishR/2019-12-31-ggplot-vonB-fitPlot-1 


################################################################################################################################################


#                                                     PRACTICE 2-PART VB MODEL: 1997


#--------- EXAMPLE PROCESS: http://derekogle.com/fishR/2019-12-31-ggplot-vonB-fitPlot-1 
data(WalleyeErie2)
wf14T <- dplyr::filter(WalleyeErie2,year==2014,sex=="female",loc==1)
vb <- vbFuns(param="Typical")
f.starts <- vbStarts(tl~age,data=wf14T)
f.fit <- nls(tl~vb(age,Linf,K,t0),data=wf14T,start=f.starts)
coef(f.fit)
f.boot1 <- Boot(f.fit)
confint(f.boot1)
predict(f.fit, data.frame(age=2:7))
predict2 <- function(x) predict(x,data.frame(age=ages))
ages <- 2:7
predict2(f.fit)
ages <- seq(-1,12,by=0.2)
f.boot2 <- Boot(f.fit,f=predict2)
preds1 <- data.frame(ages,
                     predict(f.fit,data.frame(age=ages)),
                     confint(f.boot2))
names(preds1) <- c("age","fit","LCI","UCI")
headtail(preds1)


###################################
# CHILKO PART 1: age-0.5 to age-2 #
###################################
vb1.data <- vb.dat %>%
  filter(vb_year0==1997, vb_age>=0 & vb_age <= 2,
         !is.na(vb_l)) %>%
  print()


#--------- SET STARTS AND FIT MODEL
# pre-loaded function for a typical VBGF
vb1 <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vb1.starts <- vbStarts(vb_l~vb_age, data=vb1.data)
# Linf = 135.1153  /  K = 0.6751816  /  t0 = 0.1493686

# fit and use nls() to estimate VBGF parameters from data 
vb1.fit <- nls(vb_l~vb1(vb_age, Linf, K, t0), data=vb1.data, start=vb1.starts)


#--------- MODEL PARAMTERS AND BOOTSTRAPPED COEFFICIENTS 
# extract parameter estimates
coef(vb1.fit)           # ( also shown in summary(vb1.fit) )
# Linf = 111.7418901  /  K = 1.9817982  /  t0 = 0.3515318 
confint(vb1.fit)

# bootstrapped CIs for parameter estimates   --- can be slow!
vb1.boot1 <- Boot(vb1.fit)   
headtail(vb1.boot1$coefboot, n=2)
confint(vb1.boot1, plot=T)


#--------- PREDICT 
new.prediction.df <- data.frame(vb_age = seq(0,2.5,by=0.01))

# predicted mean length-at-ages from VBGF 
predict(vb1.fit, newdata=new.prediction.df)

# predicted mean lengths-at-ages for EACH bootstrap sample 
# so that bootstrapped confidence intervals for each mean length-at-age can be derived
vb1.predict2 <- function(x) predict(x, data.frame(vb_age=seq(0,2.5,by=0.01)))
vb1.predict2(vb1.fit) 

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals) 
# It calculates the predicted mean length at all ages between -1 and 12 in increments of 0.2^2
#vb1.boot2 <- Boot(vb1.fit, f=vb1.predict2)       # can be slow
#confint(vb1.boot2, plot=T)


#------- Export
# Put in dataframe for later use
preds1 <- data.frame(vb_age = seq(0,2.5,by=0.01),
                     vb_predict_fit = predict(vb1.fit, new.prediction.df),
                     group = "1")
                     #CI=confint(vb1.boot1))


# PLOT 
ggplot() + 
  #geom_ribbon(data=preds1, aes(x=vb_age, ymin=LCI_95, ymax=UCI_95), fill="red") +
  geom_point(data=vb1.data, aes(y=vb_l,x=vb_age), size=2, alpha=0.1) +
  geom_line(data=preds1, aes(y=vb_predict_fit,x=vb_age),
            size=1, colour="red")





#################################
# CHILKO PART 2: age-3 to age-6 #
#################################
vb2.data <- vb.join %>%
  filter(vb_year0==1997, vb_age>=3 & vb_age <= 6,
         !is.na(vb_l)) %>%
  print()


#--------- SET STARTS AND FIT MODEL
# pre-loaded function for a typical VBGF
vb2 <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vb2.starts <- vbStarts(vb_l~vb_age, data=vb2.data)
# Linf = 534.9817  /  K = 1.384885  /  t0 = 2.293428

# fit and use nls() to estimate VBGF parameters from data 
vb2.fit <- nls(vb_l~vb1(vb_age, Linf, K, t0), data=vb2.data, start=vb2.starts)


#--------- MODEL PARAMTERS AND BOOTSTRAPPED COEFFICIENTS 
# extract parameter estimates
coef(vb2.fit)           # ( also shown in summary(vb1.fit) )
# Linf = 537.748131  /  K = 1.339429  /  t0 = 2.275877 
confint(vb2.fit)

# bootstrapped CIs for parameter estimates   --- can be slow!
vb2.boot1 <- Boot(vb2.fit)   
#headtail(vb2.boot1$coefboot, n=2)
confint(vb2.boot1, plot=T)


#--------- PREDICT 
new.prediction.df2 <- data.frame(vb_age = seq(2.5,7,by=0.01))

# predicted mean length-at-ages from VBGF 
predict(vb2.fit, newdata=new.prediction.df2)

# predicted mean lengths-at-ages for EACH bootstrap sample 
# so that bootstrapped confidence intervals for each mean length-at-age can be derived
vb2.predict2 <- function(x) predict(x, data.frame(vb_age = seq(2.5,7,by=0.01)))
vb2.predict2(vb2.fit) 

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals) 
# It calculates the predicted mean length at all ages between -1 and 12 in increments of 0.2^2
vb2.boot2 <- Boot(vb2.fit, f=vb2.predict2)       # can be slow
confint(vb2.boot2, plot=T)


#------- Export
# Put in dataframe for later use
preds2 <- data.frame(vb_age = seq(2.5,7,by=0.01),
                     vb_predict_fit = predict(vb2.fit, new.prediction.df2),
                     group = "2")


# PLOT 
ggplot() + 
  #geom_ribbon(data=preds2,aes(x=age,ymin=LCI,ymax=UCI),fill="gray90") +
  geom_point(data=vb2.data,aes(y=vb_l,x=vb_age), size=2, alpha=0.1) +
  geom_line(data=preds2, aes(y=vb_predict_fit,x=vb_age),
            size=1, colour="red")


#------- FULL PLOT
preds12 <- rbind(preds1, preds2)
preds.join <- full_join(preds1, preds2, by="group")

vb12.data <- vb.join%>% 
  filter(vb_year0==1997, !is.na(vb_l)) %>%
  print()

ggplot() + 
  geom_point(data=vb12.data,aes(y=vb_l, x=vb_age), size=2, alpha=0.1) +
 # geom_line(data=preds1, aes(y=vb_predict_fit, x=vb_age), size=1, colour="red") +
#  geom_line(data=preds2, aes(y=vb_predict_fit, x=vb_age), size=1, colour="red") +
  geom_line(data=preds12, aes(y=vb_predict_fit, x=vb_age, group=group), col="red") #+
  #geom_point(aes(x=2.45, y=111))




#######################################################################################################################################

#                                                          VON B CURVES

#---------------- SET UP 
# Extract years with 3 juvi years (0.5-2) and 3 adult years (3-5) - vB requires minimum 3 years of data to fit a curve 
vb_full_data_yrs <- vb.dat %>% 
  group_by(vb_year0, vb_age) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from=vb_age, values_from=n) %>%
  filter(!is.na(`0.5`), !is.na(`1`), !is.na(`2`), !is.na(`5`)) %>%
  pull(var=vb_year0)

# Subset data based on years extracted above 
# Filter out outliers that are clearly errors 
vb.dat.full <- vb.dat %>%
  filter(vb_year0 %in% c(vb_full_data_yrs)) %>%
  mutate(group = ifelse(vb_age<=2, "Freshwater phase", "Marine phase")) %>%
  filter(ifelse(vb_year0%in%c(1995, 2014), vb_age<6, vb_age>0)) %>%                   # removed the tiny 6 year old fish from 1995 & 2014
  filter(ifelse(vb_year0=="2015" & group=="Marine phase", vb_l>250, vb_l<700)) %>%     # removed the tiny 5 year old fish in 2015
  
  print()

ggplot(vb.dat.full, aes(x=vb_age, y=vb_l)) +
  geom_point() +
  facet_wrap(~vb_year0)




#=========================================================================== 
#                            JUVENILE COHORT 
#=========================================================================== 

# Note about this script: 
### There is a function line in the for() loops, "sort(names(output.fits.juvi[i]), decreasing=T)". The 'decreasing=T' argument will switch
### throughout this script between T and F - this is INTENTIONAL! For some reason, R changes the order it reads the list items in each
### loop. The model fit results were confirmed by the loop validation section below, so the differen T/F arguments are correct. 

set.seed(1)

# Group1 df
vb.dat.juvi <- vb.dat.full %>%
  filter(group=="Freshwater phase") %>%
  print()



#---------------- VISUALIZE & SUMMARY INFO FIRST 
ggplot(vb.dat.juvi, aes(x=vb_age, y=vb_l)) +
  geom_point() +
  facet_wrap(~vb_year0)

vb.dat.juvi %>%
  group_by(vb_year0) %>%
  summarize(max_l = max(vb_l))



#---------------- SET STARTS, FIT MODEL, PRINT MODEL PARAMETER ESTIMATES (COEF AND CONFINT)
# pre-loaded function for a typical VBGF
vb <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vbstarts.juvi <- split(vb.dat.juvi, vb.dat.juvi$vb_year0) %>% 
  map(~vbStarts(vb_l ~ vb_age, method="oldAge", data =.))

# Fit and use nls() to estimate VBGF parameters from data 
output.fits.juvi <- replicate(length(vbstarts.juvi), NULL) 
split.data.juvi <- split(vb.dat.juvi, vb.dat.juvi$vb_year0)

for(i in 1:length(unique(vbstarts.juvi))){
  output.fits.juvi[[i]] <- nls(vb_l~vb(vb_age, Linf, K, t0), data=split.data.juvi[[i]], start=vbstarts.juvi[[i]])
  names(output.fits.juvi) <- sort(names(vbstarts.juvi), decreasing=T)
  print(coef(output.fits.juvi[[i]]))            # print() just to confirm loop works - can be omitted 
  #print(confint(output.fits.juvi[[i]]))        # print() just to confirm loop works - can be omitted
}
output.fits.juvi[[1]]

# Extract fitted model parameter estimates into df 
param.fits.juvi <- replicate(length(output.fits.juvi), NULL) 
param.CIs.juvi <- replicate(length(output.fits.juvi), NULL) 

for(i in 1:length(unique(output.fits.juvi))){                # this loop could be better
  param.fits.juvi[i]<-list(as.data.frame(coef(output.fits.juvi[[i]])))
  names(param.fits.juvi) <- sort(names(output.fits.juvi), decreasing=T)
  param.fits.juvi.df <- param.fits.juvi %>% bind_rows()
  
  param.CIs.juvi[i]<-list(as.data.frame(confint(output.fits.juvi[[i]])))
  names(param.CIs.juvi) <- sort(names(output.fits.juvi), decreasing=T)
  param.CIs.juvi.df <- param.CIs.juvi %>% bind_rows()
}

# Create df for future use 
fits.CIs.juvi.df <- cbind(param.fits.juvi.df, param.CIs.juvi.df) %>%
  rownames_to_column(var="param") %>%
  mutate(param=case_when(grepl("Linf",param)~"Linf", grepl("K",param)~"k", grepl("t0",param)~"t0")) %>%
  rename(estimate=`coef(output.fits.juvi[[i]])`, LCI=`2.5%`, UCI=`97.5%`) %>%
  mutate(vb_year0=rep(names(output.fits.juvi), each=3)) %>%
  mutate_at(vars("param", "vb_year0"), as.factor) %>%
  mutate(group = "Freshwater phase")


#---------------- BOOTSTRAPPED COEFFICIENTS/PARAMETERS 
# Bootstrapped CIs for INDIVIDUAL parameter estimates      ** VERY SLOW **
boot1.juvi <- replicate(length(output.fits.juvi), NULL) 
for(i in 1:length(unique(output.fits.juvi))){
  boot1.juvi[[i]] <- Boot(output.fits.juvi[[i]]) 
  names(boot1.juvi) <- sort(names(output.fits.juvi), decreasing=F)
}

confint(boot1.juvi[[1]])



#---------------- PREDICT CURVES WITH BOOTSTRAPPED 95% CIs 
# Create dataframe of ages for which to predict lengths 
pred.juvi.ages <- data.frame(vb_age = seq(0,2.5,by=0.1))

####### PREDICTED CURVES
# Predicted mean length-at-ages from VBGF 
predict.juvi <- replicate(length(output.fits.juvi), NULL) 
for(i in 1:length(unique(output.fits.juvi))){
  predict.juvi[[i]] <- predict(output.fits.juvi[[i]], newdata=pred.juvi.ages)
  names(predict.juvi) <- sort(names(output.fits.juvi), decreasing=F)
}
# convert from List to dataframe for use later
pred.juvi.df <- predict.juvi %>% 
  bind_rows() %>% 
  pivot_longer(cols=c(1:13), names_to="vb_year0", values_to="pred_fit") %>%
  mutate(vb_age = rep(seq(0,2.5,by=0.1), each=13),
         group="Freshwater phase") %>%
  arrange(vb_year0)
####### 


####### PREDICTED CIs
# predicted mean lengths-at-ages for EACH bootstrap sample 
# so that bootstrapped confidence intervals for each mean length-at-age can be derived
predict2.juvi.fx <- function(x) predict(x, data.frame(vb_age = seq(0,2.5,by=0.1)))
predict2.juvi.fx(output.fits.juvi[[1]])       # test function

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals)    *** VERY SLOW ***  
# It calculates the predicted mean length at all ages between 0 and 2.5 by 0.1
bootCI.juvi <- replicate(length(output.fits.juvi), NULL)
for(i in 1:length(unique(output.fits.juvi))){
  bootCI.juvi[[i]] <- Boot(output.fits.juvi[[i]], f=predict2.juvi.fx)
  names(bootCI.juvi) <- sort(names(output.fits.juvi), decreasing=F)
}
#View(Confint(bootCI.juvi[[1]]))               # test loop


# Extract 95% CIs from Boot() object and store in list    *** VERY SLOW ***  
predict.juvi.CI <- replicate(length(output.fits.juvi), NULL)
for(i in 1:length(unique(bootCI.juvi))){
  predict.juvi.CI[i]<-list(as.data.frame(Confint(bootCI.juvi[[i]])))
  names(predict.juvi.CI) <- sort(names(output.fits.juvi), decreasing=F)
}

# convert from List to dataframe for use later
predict.juvi.CI.df <- predict.juvi.CI %>% 
  bind_rows() %>% 
  remove_rownames() %>%
  mutate(vb_year0=rep(sort(names(output.fits.juvi), decreasing=F), each=26)) %>%
  rename(LCI=`2.5 %`,
         UCI=`97.5 %`) %>%
  arrange(vb_year0)
####### 


#---------------- COMBINE AND PLOT
pred.juvi.df2 <- cbind(pred.juvi.df, predict.juvi.CI.df) %>% select(1:6)

# PLOT 
ggplot() +  geom_ribbon(data=pred.juvi.df2, aes(x=vb_age, ymin=LCI, ymax=UCI),fill="lime green") +
  geom_point(data=vb.dat.juvi, aes(x=vb_age, y=vb_l)) +
  geom_line(data=pred.juvi.df2, aes(x=vb_age, y=pred_fit), size=0.5, colour="red") +
  facet_wrap(~vb_year0)  





#=========================================================================== 
#                            ADULT COHORT 
#=========================================================================== 

# Note about this script: 
### There is a function line in the for() loops, "sort(names(output.fits.juvi[i]), decreasing=T)". The 'decreasing=T' argument will switch
### throughout this script between T and F - this is INTENTIONAL! For some reason, R changes the order it reads the list items in each
### loop. The model fit results were confirmed by the loop validation section below, so the different T/F arguments are correct. 

set.seed(2)

# Adult df
vb.dat.adult <- vb.dat.full %>%
  filter(group=="Marine phase") %>% 
  print()

#---------------- VISUALIZE & SUMMARY INFO FIRST 
ggplot(vb.dat.adult, aes(x=vb_age, y=vb_l)) +
  geom_point() +
  facet_wrap(~vb_year0)

vb.dat.adult %>%
  group_by(vb_year0) %>%
  summarize(max_l = max(vb_l))



#---------------- SET STARTS, FIT MODEL, PRINT MODEL PARAMETER ESTIMATES (COEF AND CONFINT)
# pre-loaded function for a typical VBGF
vb <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vbstarts.adult <- split(vb.dat.adult, vb.dat.adult$vb_year0) %>% 
  map(~vbStarts(vb_l ~ vb_age, method="oldAge", data =.))

# Fit and use nls() to estimate VBGF parameters from data 
output.fits.adult <- replicate(length(vbstarts.adult), NULL) 
split.data.adult <- split(vb.dat.adult, vb.dat.adult$vb_year0)

for(i in 1:length(unique(vbstarts.adult))){
  output.fits.adult[[i]] <- nls(vb_l~vb(vb_age, Linf, K, t0), data=split.data.adult[[i]], start=vbstarts.adult[[i]])
  names(output.fits.adult) <- sort(names(vbstarts.adult), decreasing=T)
  print(coef(output.fits.adult[[i]]))             # print() just to confirm loop works - can be omitted
  #print(confint(output.fits.adult[[i]]))         # print() just to confirm loop works - can be omitted
}

# Extract fitted model parameter estimates into df 
param.fits.adult <- replicate(length(output.fits.adult), NULL) 
param.CIs.adult <- replicate(length(output.fits.adult), NULL) 

for(i in 1:length(unique(output.fits.adult))){
  param.fits.adult[i]<-list(as.data.frame(coef(output.fits.adult[[i]])))
  names(param.fits.adult) <- sort(names(output.fits.adult), decreasing=T)
  param.fits.adult.df <- param.fits.adult %>% bind_rows()
  
  param.CIs.adult[i]<-list(as.data.frame(confint(output.fits.adult[[i]])))
  names(param.CIs.adult) <- sort(names(output.fits.adult), decreasing=T)
  param.CIs.adult.df <- param.CIs.adult %>% bind_rows()
}

# Create df for future use 
fits.CIs.adult.df <- cbind(param.fits.adult.df, param.CIs.adult.df) %>%
  rownames_to_column(var="param") %>%
  mutate(param=case_when(grepl("Linf",param)~"Linf", grepl("K",param)~"k", grepl("t0",param)~"t0")) %>%
  rename(estimate=`coef(output.fits.adult[[i]])`, LCI=`2.5%`, UCI=`97.5%`) %>%
  mutate(vb_year0=rep(names(output.fits.adult), each=3)) %>%
  mutate_at(vars("param", "vb_year0"), as.factor) %>%
  mutate(group = "Marine phase")



#---------------- BOOTSTRAPPED COEFFICIENTS/PARAMETERS 
# Bootstrapped CIs for parameter estimates           *** VERY SLOW ***  
boot1.adult <- replicate(length(output.fits.adult), NULL) 
for(i in 1:length(unique(output.fits.adult))){
  boot1.adult[[i]] <- Boot(output.fits.adult[[i]]) 
  names(boot1.adult) <- sort(names(output.fits.adult), decreasing=F)
}

confint(boot1.adult[[2]])



#---------------- PREDICT CURVES WITH BOOTSTRAPPED 95% CIs 
####### PREDICTED CURVES
pred.adult.df <- data.frame(vb_age = seq(2.5,7,by=0.1))

# Predicted mean length-at-ages from VBGF (i.e., THE CURVE)
predict.adult <- replicate(length(output.fits.adult), NULL) 
for(i in 1:length(unique(output.fits.adult))){
  predict.adult[[i]] <- predict(output.fits.adult[[i]], newdata=pred.adult.df)
  names(predict.adult) <- sort(names(output.fits.adult), decreasing=F)
}
pred.adult.df <- predict.adult %>% 
  bind_rows() %>% 
  pivot_longer(cols=c(1:13), names_to="vb_year0", values_to="pred_fit") %>%
  mutate(vb_age = rep(seq(2.5,7,by=0.1), each=13),
         group="Marine phase") %>%
  arrange(vb_year0)
####### 

####### PREDICTED CIs
# Predicted mean lengths-at-ages FUNCTION for EACH bootstrap sample (i.e., THE RIBBON)
predict2.adult.fx <- function(x) predict(x, data.frame(vb_age = seq(2.5,7,by=0.1)))
predict2.adult.fx(output.fits.adult[[2]]) 

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals) 
# It calculates the predicted mean length at all ages between 2.5 and 7 by 0.1    *** VERY SLOW ***  
bootCI.adult <- replicate(length(output.fits.adult), NULL)
for(i in 1:length(unique(output.fits.adult))){
  bootCI.adult[[i]] <- Boot(output.fits.adult[[i]], f=predict2.adult.fx)
  names(bootCI.adult) <- sort(names(output.fits.adult), decreasing=F)
}
#View(Confint(bootCI.adult[[2]]))             # test loop, VERY SLOW


# Extract 95% CIs from Boot() object and store in list   *** VERY SLOW ***  
predict.adult.CI <- replicate(length(output.fits.adult), NULL)
for(i in 1:length(unique(bootCI.adult))){
  predict.adult.CI[i]<-list(as.data.frame(Confint(bootCI.adult[[i]])))
  names(predict.adult.CI) <- sort(names(output.fits.adult), decreasing=F)
}

# convert from List to dataframe for use later
predict.adult.CI.df <- predict.adult.CI %>% 
  bind_rows() %>% 
  remove_rownames() %>%
  mutate(vb_year0=rep(sort(names(output.fits.adult), decreasing=F), each=46)) %>%
  rename(LCI=`2.5 %`,
         UCI=`97.5 %`) %>%
  arrange(vb_year0)
####### 


#---------------- COMBINE AND PLOT
pred.adult.df2 <- cbind(pred.adult.df, predict.adult.CI.df) %>% select(1:6)

# PLOT 
ggplot() +
  geom_ribbon(data=pred.adult.df2, aes(x=vb_age, ymin=LCI, ymax=UCI),fill="red", alpha=0.3) +
  geom_point(data=vb.dat.adult, aes(x=vb_age, y=vb_l), shape=21, size=2, alpha=0.6, fill="gray30", colour="gray30") +
  geom_line(data=pred.adult.df2, aes(x=vb_age, y=pred_fit), size=1, colour="red") +
  facet_wrap(~vb_year0) 




#=========================================================================== 
#                              FULL PLOT
#=========================================================================== 

# Combine DFs - EXPORT FOR RMARKDOWN USE
vb.fit.df <- rbind(pred.juvi.df2, pred.adult.df2)
write.csv(vb.fit.df, "vonB_model_fits.csv", row.names=F)
write.csv(rbind(vb.dat.juvi, vb.dat.adult), "vonB_model_fits_data.csv", row.names=F)

# PLOT
ggplot() +
  geom_point(data=vb.dat.juvi, aes(x=vb_age, y=vb_l), shape=21, size=2.5, stroke=1.3, alpha=0.2, fill="black", colour="black") +
  geom_point(data=vb.dat.adult, aes(x=vb_age, y=vb_l), shape=21, size=2.5, stroke=1.3, alpha=0.2, fill="black", colour="black") +
  geom_ribbon(data=vb.fit.df, aes(x=vb_age, ymin=LCI, ymax=UCI, group=group), fill="red", alpha=0.2) +
  geom_line(data=vb.fit.df, aes(x=vb_age, y=pred_fit, group=group), size=0.7, colour="red") +
  labs(x="Age", y="Length (mm)", 
       caption="Fig 1. Fitted von Bertalanffy growth models with bootstrapped 95% confidence intervals for freshwater (fry and smolt) and marine (including jacks) \nsockeye from Chilko River.") +
  facet_wrap(~vb_year0) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=13),
        panel.grid.major = element_line(colour="gray82", size=0.1),
        panel.grid.minor = element_line(colour="gray82", size=0.1),
        plot.caption = element_text(hjust=0, face="italic", size=11))




########################################################################################################################################## 

#                                                        LOOP CODE VALIDATION 


#========================= 
#         1991 
#=========================

#group1 df
vbdat.91 <- smolt.vbdat.full %>%
  filter(vb_year0=="1991", group=="1") %>%
  print()


#--------- SET STARTS AND FIT MODEL
# pre-loaded function for a typical VBGF
vb <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vbstarts91 <- vbStarts(vb_l ~ vb_age, method="oldAge", data =vbdat.91)

# Fit and use nls() to estimate VBGF parameters from data 
vbfit91 <- nls(vb_l~vb(vb_age, Linf, K, t0), data=vbdat.91, start=vbstarts91)


#--------- MODEL PARAMTERS AND BOOTSTRAPPED COEFFICIENTS 
# extract parameter estimates
coef(vbfit91)           
confint(vbfit91)        

# bootstrapped CIs for parameter estimates   --- can be slow!
boot91.1 <- Boot(vbfit91) 
confint(boot91.1)
confint(boot91.1)



#--------- PREDICT 
pred91df <- data.frame(vb_age = seq(0,2.5,by=0.01))

# predicted mean lengths-at-ages for EACH bootstrap sample 
# so that bootstrapped confidence intervals for each mean length-at-age can be derived
group91.predict2 <- function(x) predict(x, data.frame(vb_age = seq(0,2.5,by=0.01)))
group91.predict2(vbfit91) 

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals) 
# It calculates the predicted mean length at all ages between -1 and 12 in increments of 0.2^2
group91.boot2 <- Boot(vbfit91, f=group91.predict2)       # can be slow
Confint(group91.boot2, plot=T)



#------- EXPORT AND PLOT
# Put in dataframe for later use
preds91df2 <- data.frame(
  vb_age = seq(0,2.5,by=0.01),
  pred_fit91 = predict(vbfit91, newdata=pred91df),
  vb_year0 = "1991",
  Confint(group91.boot2))
preds91df2 <- preds91df2 %>% 
  rename(LCI=`X2.5..`,
         UCI=`X97.5..`)




#========================= 
#         1993 
#=========================

#group1 df
vbdat.93 <- smolt.vbdat.full %>%
  filter(vb_year0=="1993", group=="1") %>%
  print()


#--------- SET STARTS AND FIT MODEL
# pre-loaded function for a typical VBGF
vb <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vbstarts93 <- vbStarts(vb_l ~ vb_age, method="oldAge", data =vbdat.93)

# Fit and use nls() to estimate VBGF parameters from data 
vbfit93 <- nls(vb_l~vb(vb_age, Linf, K, t0), data=vbdat.93, start=vbstarts93)


#--------- MODEL PARAMTERS AND BOOTSTRAPPED COEFFICIENTS 
# extract parameter estimates
coef(vbfit93)           
confint(vbfit93)        

# bootstrapped CIs for parameter estimates   --- can be slow!
boot93.1 <- Boot(vbfit93) 
confint(boot93.1)
confint(boot93.1)



#--------- PREDICT 
pred93df <- data.frame(vb_age = seq(0,2.5,by=0.01))

# predicted mean lengths-at-ages for EACH bootstrap sample 
# so that bootstrapped confidence intervals for each mean length-at-age can be derived
group93.predict2 <- function(x) predict(x, data.frame(vb_age = seq(0,2.5,by=0.01)))
group93.predict2(vbfit93) 

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals) 
# It calculates the predicted mean length at all ages between -1 and 12 in increments of 0.2^2
group93.boot2 <- Boot(vbfit93, f=group1.predict2)       # can be slow
Confint(group1.boot2)



#------- EXPORT AND PLOT
# Put in dataframe for later use
preds93df2 <- data.frame(
  vb_age = seq(0,2.5,by=0.01),
  pred_fit93 = predict(vbfit93, newdata=pred93df),
  vb_year0 = "1993",
  Confint(group93.boot2))
preds93df2 <- preds93df2 %>% 
  rename(LCI=`X2.5..`,
         UCI=`X97.5..`)




#========================= 
#         1996 
#=========================

#group2 df
vbdat.96 <- smolt.vbdat.full %>%
  filter(vb_year0=="1996", group=="2", !is.na(vb_l)) %>%
  print()


#--------- SET STARTS AND FIT MODEL
# pre-loaded function for a typical VBGF
vb <- vbFuns(param="Typical")

# Extract reasonable starting values from data
# note 'vb_year' is a user-defined function that filters the data to select ages 0.5 to 2 (inclusive) only
vbstarts96 <- vbStarts(vb_l ~ vb_age, data=vbdat.96, methLinf="oldAge")

# Fit and use nls() to estimate VBGF parameters from data 
vbfit96 <- nls(vb_l~vb(vb_age, Linf, K, t0), data=vbdat.96, start=vbstarts96)


#--------- MODEL PARAMTERS AND BOOTSTRAPPED COEFFICIENTS 
# extract parameter estimates
coef(vbfit96)           
confint(vbfit96)        

# bootstrapped CIs for parameter estimates   --- can be slow!
boot96.1 <- Boot(vbfit96) 
confint(boot96.1)



#--------- PREDICT 
pred96df <- data.frame(vb_age = seq(2.5,7,by=0.01))

# predicted mean lengths-at-ages for EACH bootstrap sample 
# so that bootstrapped confidence intervals for each mean length-at-age can be derived
group96.predict2 <- function(x) predict(x, data.frame(vb_age = ages))
group96.predict2(vbfit96) 

# Construct predicted mean lengths-at-age (with bootstrapped confidence intervals) 
# It calculates the predicted mean length at all ages between -1 and 12 in increments of 0.2^2
ages<-seq(2.5,7,by=0.01)
group96.boot2 <- Boot(vbfit96, f=group96.predict2)       # can be slow
Confint(group96.boot2)                                   # very slow


#------- EXPORT AND PLOT
# Put in dataframe for later use
preds96df2 <- data.frame(
  vb_age = seq(2.5,7,by=0.01),
  pred_fit96 = predict(vbfit96, newdata=pred96df),
  vb_year0 = "1996",
  Confint(group96.boot2))
preds96df2 <- preds96df2 %>% 
  rename(LCI=`X2.5..`,
         UCI=`X97.5..`)




#========================= 
#         PLOT  
#========================= 

ggplot() +
  geom_point(data=vb.dat.juvi, aes(x=vb_age, y=vb_l)) +
  geom_point(data=vb.dat.adult, aes(x=vb_age, y=vb_l)) +
  geom_ribbon(data=preds91df2, aes(x=vb_age, ymin=LCI, ymax=UCI), fill="gray60") +
  geom_ribbon(data=preds93df2, aes(x=vb_age, ymin=LCI, ymax=UCI), fill="gray60") +
  geom_ribbon(data=preds96df2, aes(x=vb_age, ymin=LCI, ymax=UCI), fill="gray60") +
  geom_line(data=preds91df2, aes(x=vb_age, y=pred_fit91), colour="lime green", linetype="dashed", size=2, alpha=0.7) +  
  geom_line(data=preds93df2, aes(x=vb_age, y=pred_fit93), colour="lime green", linetype="dotted", size=2, alpha=0.7) +
  geom_line(data=preds96df2, aes(x=vb_age, y=pred_fit96), colour="lime green", linetype="dotted", size=2, alpha=0.7) +  
  facet_wrap(~vb_year0)  



########################################################################################################################################## 


#                                                     TESTING K OVER TIME AND AMONG COHORTS

vonB.fits.CIs <- rbind(fits.CIs.adult.df, fits.CIs.juvi.df)
write.csv(vonB.fits.CIs, "vonB_model_parameters.csv", row.names=F)

# Visually examine 
ggplot(vonB.fits.CIs, aes(x=vb_year0, y=estimate, group=interaction(group,param), fill=group)) +
  geom_errorbar(aes(x=vb_year0, ymax=UCI, ymin=LCI), width=0.1, size=1) +
  geom_point(shape=21, size=3, stroke=1.5) +
  facet_wrap(~param, nrow=3, scales = "free_y") +
  theme_bw()


# TEST OF SIGNIFICANCE: k
k.dat <- vonB.fits.CIs %>% filter(param=="k")
lm.k <- aov(estimate ~ group + vb_year0, data=k.dat)
r.k <- resid(lm.k)
hist(r.k)
qqnorm(r.k)
qqline(r.k)
summary(lm.k)
TukeyHSD(lm.k)









