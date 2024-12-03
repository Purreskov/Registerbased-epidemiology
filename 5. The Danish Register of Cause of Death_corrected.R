#### Register Based Epidemiology
#### The Danish Cause of Death Register
#### 14, nov, 2024

### survival analysis (time between baseline and enddate, status)

### step 1. read DCH & CPR data and merge them
### step 2. define enddate and time from CPR
### step 3. read cause of death data and merge with DCH & CPR data
### step 4. update status and enddate for each cause
### step 5. mortality rate and mortality rate ratio
### step 6. cox models



# 1)	Get an overview of the variables in the dataset Cause of Death Register.dta
### load the package
library(haven)
library(dplyr)
library(readxl)
library(ggplot2)
library(data.table)
library(survival)

# set your working directory
rm(list = ls())
setwd("/Users/gustavpurreskov/Documents/Uni/Kandidat/1. semester/Registerbased Epidemiology/Exercises/Ex-5_nov-12-14")


##########################################################
### step 1. read DCH & CPR data and merge them
##########################################################
dch <- read_dta("/Users/gustavpurreskov/Documents/Uni/Kandidat/1. semester/Registerbased Epidemiology/Exercises/Ex-2_oct-24/dch.dta")
dim(dch)
vitalstatus <- read_dta("/Users/gustavpurreskov/Documents/Uni/Kandidat/1. semester/Registerbased Epidemiology/Exercises/Ex-4_oct-31/vitalstatus2009.dta")
dim(vitalstatus)
dch.cpr <- merge(dch, vitalstatus, by = "id")

##########################################################
### step 2. define enddate and time from CPR
##########################################################
### end of study is '2009-12-31'
### if death, immigration, disappear, enddate is updated
table(is.na(dch.cpr$SDATO2009))
tail(table(dch.cpr$SDATO2009))

# status
dch.cpr$dead <- ifelse(dch.cpr$STATUS2009 == 90, 1, 0)
table(dch.cpr$dead)

# time
table(dch.cpr$STATUS2009)
class(dch.cpr$SDATO2009) # date
dch.cpr$enddate <- ifelse(dch.cpr$STATUS2009 %in% c(60, 80, 90),
  as.character(dch.cpr$SDATO2009),
  "2009-12-31"
)
class(dch.cpr$enddate)
# we need to change it to Date format
dch.cpr$enddate <- as.Date(dch.cpr$enddate, origin = "1970-01-01")

# calculate time
dch.cpr$time <- (dch.cpr$enddate - dch.cpr$mdato) / 365.25 # leap year every 4 years
class(dch.cpr$time)
dch.cpr$time <- as.numeric(dch.cpr$time)
class(dch.cpr$time)

tapply(dch.cpr$time, dch.cpr$dead, sum)
View(dch.cpr)

##########################################################
### step 3. read cause of death data and merge with DCH & CPR data
##########################################################

# 1) Load the dataset
causeofdeath <- read_dta("/Users/gustavpurreskov/Documents/Uni/Kandidat/1. semester/Registerbased Epidemiology/Exercises/Ex-5_nov-12-14/causeofdeath.dta")

# a.	How many variables and observations are in the dataset?
dim(causeofdeath)
summary(causeofdeath)
View(causeofdeath)

# b.	What is the most frequent underlying cause of death?
table(causeofdeath$daars1)

COD <- table(causeofdeath$daars1)
tail(COD[order(COD)])
# C189 C509 C259 C619 I219 C349
#   47   48   56   56  102  219

# c.	How are the different underlying causes of death (ICD codes) distributed?
barplot(table(causeofdeath$daars1), las = 2)
barplot(table(substr(causeofdeath$daars1, 1, 3)))
barplot(table(substr(causeofdeath$daars1, 1, 2)))
barplot(table(substr(causeofdeath$daars1, 1, 1)))

# d.	How are the contributory causes of death (ICD codes) distributed in total and for each?
names(causeofdeath)

table(causeofdeath$daars2)

# win.graph(w = 12) # Only for windows users
par(mfrow = c(2, 3))
barplot(table(substr(causeofdeath$daars1, 1, 1)), main = "daars1", las = 2)
barplot(table(substr(causeofdeath$daars2, 1, 1)), main = "daars2", las = 2)
barplot(table(substr(causeofdeath$daars3, 1, 1)), main = "daars3", las = 2)
barplot(table(substr(causeofdeath$daars4, 1, 1)), main = "daars4", las = 2)
barplot(table(substr(causeofdeath$daars5, 1, 1)), main = "daars5", las = 2)

# e.	How many observations in the dataset have missing information on cause of death?
table(is.na(causeofdeath$daars1))
table(causeofdeath$daars1 == "")

# 2)	Merge the dataset with causes of deaths (causeofdeath.dta) and the cohort data (dch.cpr.rds)
dch.cpr.death <- merge(dch.cpr, causeofdeath, by = "id", all.x = T)
View(head(dch.cpr.death))

# a.	How many of the cohort participants, who died before 31.12.2009, have a cause of death?
table(is.na(dch.cpr.death$daars1) | dch.cpr.death$daars1 == "")

# b.	Among cohort participants who died, how many (number and percentages) died due to lung cancer
# (ICD-10 code C33 & C34) as underlying cause of death until 31.12.2009?
table(dch.cpr.death$dead)
table(dch.cpr.death$daars1, exclude = NULL)


##########################################################
### step 4. update status and enddate for each cause
##########################################################
###### SUPER IMPORTANT PART OF THE CODE ++++++ #####

# save the first 3 digits of ICD10 code
dch.cpr.death$ICD10.3 <- substr(dch.cpr.death$daars1, 1, 3)

# Examples of using substr to extract parts of a string
substr("I219", start = 1, stop = 3)
substr("I219", start = 4, stop = 4) # Only the 4th caracter

# Create a frequency table of the first 3 digits of ICD10 codes, 
# including NA values (people who didn't died)
table(dch.cpr.death$ICD10.3, exclude = NULL)

# Identify lung cancer is C33 or C34 
# (note: %in%, simillar to ==, but it can match multiples values and return FALSE for NA)
dch.cpr.death$lungcancer <- ifelse(dch.cpr.death$ICD10.3 %in% c("C33", "C34"),
  1,
  0
)

# Check the ICD-10 codes we subset for lung cancer
table(dch.cpr.death$daars1[dch.cpr.death$lungcancer == 1])

# Save the first character of the ICD10 code
dch.cpr.death$ICD10.1rst.char <- substr(dch.cpr.death$daars1, 1, 1) #keep 1
table(dch.cpr.death$ICD10.1rst.char)

# Identify all cancer cases (ICD10 codes starting with C or D)
dch.cpr.death$allcancer <- ifelse(
  dch.cpr.death$ICD10.1rst.char %in% c("C", "D"),
  1,
  0
)


###### END OF SUPER IMPORTANT PART OF THE CODE ++++++ #####
# Identify myocardial infarction cases (ICD10 code I21)
dch.cpr.death$mi <- ifelse(
  dch.cpr.death$ICD10.3 %in% c("I21"),
  1,
  0
)

# Create frequency tables for lung cancer and myocardial infarction
table(dch.cpr.death$lungcancer)
table(dch.cpr.death$mi)
# 235 died from lung cancer

# Calculate the percentage of lung cancer cases
prop.table(table(dch.cpr.death$lungcancer)) * 100
# 1.41%

# Frequency of lung cancer among the deceased:
# It's always a good idea to first look at the frequency table before using prop.table,
# in order to be sure of the proportion denominator
# Create a frequency table for lung cancer among the deceased
table(dch.cpr.death$lungcancer[dch.cpr.death$dead == 1])

# Among all causes of death, calculate the percentage who died from lung cancer
tmp <- dch.cpr.death[dch.cpr.death$dead == 1, ]
prop.table(table(tmp$lungcancer)) * 100
# or
prop.table(table(dch.cpr.death[dch.cpr.death$dead == 1, ]$lungcancer)) * 100
# 14.01%


##########################################################
### step 5. mortality rate and mortality rate ratio
##########################################################

#### > All cause, overall ####

### mortality rate for all-cause mortality
# status
table(dch.cpr.death$dead)
# time
sum(dch.cpr.death$time)

### mortality rate for lung cancer mortality
# status
table(dch.cpr.death$lungcancer)
# time
sum(dch.cpr.death$time)

#### > Lung cancer specific, overall ####

# c.	Calculate the overall and lung cancer specific mortality rate for this cohort.
# all-cause
table(dch.cpr.death$dead)[2] / sum(dch.cpr.death$time)
# 0.0076698

# lung cancer
table(dch.cpr.death$lungcancer)[2] / sum(dch.cpr.death$time)
# 0.0010748

table(dch.cpr.death$lungcancer)[2] / sum(dch.cpr.death$time) * 100000


#### > Lung cancer specific, sex ####

# d.	Calculate the mortality rate for lung cancer for men and women separately
table(dch.cpr.death$lungcancer, dch.cpr.death$kqn)

# male
tmp_male <- dch.cpr.death[dch.cpr.death$kqn == "M", ] # This subset the dataset to men only, remember the coma
table(tmp_male$lungcancer)
sum(tmp_male$time)

table(tmp_male$lungcancer)[2] / sum(tmp_male$time) * 100000 # Mortality rate for men

# female
tmp_female <- dch.cpr.death[dch.cpr.death$kqn == "F", ]  # This subset the dataset to female only
table(tmp_female$lungcancer)
sum(tmp_female$time)

# Thi
MMRT <- dch.cpr.death %>%
  group_by(kqn) %>%
  summarise(MMR = sum(lungcancer) / sum(time))
MMRT
#  F     0.000901
#  M     0.00126

# What is mortality rate ratio of male compared to female?
0.00126 / 0.000901
# Or
MMRT[2, 2] / MMRT[1, 2]
# 1.396804


#### > Lung cancer specific, marital status ####

# Mortatlity rate for marital status
dch.cpr.death$married

table(dch.cpr.death$married, useNA = "always")  # There is missing in marital status
sum(is.na(dch.cpr.death$married))

subset_married <- dch.cpr.death[dch.cpr.death$married == 2, ] 
table(subset_married$lungcancer)
sum(subset_married$time) # This will fail because of the missing

# We can fix fix in two ways
sum(subset_married$time, na.rm = TRUE) # or
subset_married <- dch.cpr.death[dch.cpr.death$married %in% 2, ]

##########################################################
### step 6. cox models
##########################################################

### generate covariates
names(dch.cpr.death)
dch.cpr.death$smoking <- factor(dch.cpr.death$smoking,
  levels = c(1, 2, 3),
  labels = c("never smokers", "previous smokers", "current smokers")
)
dch.cpr.death$eversmok <- ifelse(dch.cpr.death$smoking == "never smokers", 0, 1)
dch.cpr.death$bmicat <- ifelse(dch.cpr.death$bmi < 25, 1, 2)
dch.cpr.death$obesity <- ifelse(dch.cpr.death$bmi < 30, 1, 2)
dch.cpr.death$sex <- factor(dch.cpr.death$kqn,
  levels = c("F", "M")
)
dch.cpr.death$age_cat <- ifelse(dch.cpr.death$age > 55, ">55", "<55")

# 3)	Use cox proportional hazards regression model to obtain the hazard ratio of
# dying from lung cancer for males compared to females
cox.s <- coxph(Surv(time, lungcancer) ~ sex, data = dch.cpr.death)
summary(cox.s)
#      exp(coef) exp(-coef) lower .95 upper .95
# sexM     1.402     0.7131     1.083     1.816
# 1.402 (1.083, 1.816)

# Compare to RR=1.3968
# KM estimator, censors

# 4)	Use cox proportional hazards regression model to obtain the hazard ratio of dying
# from lung cancer for males compared to females in an age adjusted model
cox.s.a <- coxph(Surv(time, lungcancer) ~ sex + age, data = dch.cpr.death)
summary(cox.s.a)
#      exp(coef) exp(-coef) lower .95 upper .95
# sexM     1.431     0.6987     1.105     1.853
# age      1.127     0.8873     1.095     1.160
# Compare to RR=1.402 in raw model

# 5)	What is the hazard ratio of dying from lung cancer for an ever smoker compared to
# a never smoker in a model adjusted for age and sex?
cox.smok <- coxph(Surv(time, lungcancer) ~ factor(eversmok), data = dch.cpr.death)
summary(cox.smok)
#                   exp(coef) exp(-coef) lower .95 upper .95
# factor(eversmok)1     10.83    0.09237     6.056     19.35

cox.smok.a.s <- coxph(Surv(time, lungcancer) ~ factor(eversmok) + sex + age, data = dch.cpr.death)
summary(cox.smok.a.s)
#                   exp(coef) exp(-coef) lower .95 upper .95
# factor(eversmok)1     9.881     0.1012    5.5107    17.716
# sexM                  1.108     0.9027    0.8546     1.436
# age                   1.114     0.8974    1.0829     1.147


cox.smok.a_cat <- coxph(Surv(time, lungcancer) ~ age_cat, data = dch.cpr.death)
summary(cox.smok.a_cat)

# What happen if we change the reference
dch.cpr.death$age_cat <- factor(dch.cpr.death$age_cat, levels = c(">55", "<55"))
cox.smok.a_cat <- coxph(Surv(time, lungcancer) ~ age_cat, data = dch.cpr.death)
summary(cox.smok.a_cat)



# 6)	Save the dataset as dch.cpr.death.rds
saveRDS(dch.cpr.death, "/Users/gustavpurreskov/Documents/Uni/Kandidat/1. semester/Registerbased Epidemiology/Exercises/Ex-5_nov-12-14/dch.cpr.death.rds")

