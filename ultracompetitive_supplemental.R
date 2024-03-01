# This script performs the volatility analyses for the gender and competitiveness paper

# Loading libraries

library(modelsummary)
library(stargazer)
library(ggplot2)
library(dplyr)

# Project directory

direc <- 'D:/ultracompetitive/'

# Reading in the data

vd <- read.csv(paste0(direc, 'volatility_data.csv'))
rp <- read.csv(paste0(direc, 'rpx.csv'))

# Calculate the coefficient of variation for each runner

vd$CV <- vd$Volatility / vd$Ability

# Volatility analysis

vm1 <- lm(CV ~ Gender + log(Age) + log(Altitude) + Some_HS + HS  +Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 10),])

vm2 <- lm(CV ~ Gender*log(Age) + log(Altitude) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 10),])

vm3 <- lm(CV ~ Gender + Age + log(Altitude) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 10),])

vm4 <- lm(CV ~ Gender*Age + log(Altitude) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 10),])

vm5 <- lm(CV ~ Gender + log(Age) + log(Altitude) + Some_HS + HS  +Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 2),])

vm6 <- lm(CV ~ Gender*log(Age) + log(Altitude) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 2),])

vm7 <- lm(CV ~ Gender + Age + log(Altitude) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 2),])

vm8 <- lm(CV ~ Gender*Age + log(Altitude) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = vd[which(vd$Races >= 2),])

vm1x <- coeftest(vm1, vcov = vcovCL(vm1, type = 'HC1'))
vm2x <- coeftest(vm2, vcov = vcovCL(vm2, type = 'HC1'))
vm3x <- coeftest(vm3, vcov = vcovCL(vm3, type = 'HC1'))
vm4x <- coeftest(vm4, vcov = vcovCL(vm4, type = 'HC1'))

vm5x <- coeftest(vm5, vcov = vcovCL(vm5, type = 'HC1'))
vm6x <- coeftest(vm6, vcov = vcovCL(vm6, type = 'HC1'))
vm7x <- coeftest(vm7, vcov = vcovCL(vm7, type = 'HC1'))
vm8x <- coeftest(vm8, vcov = vcovCL(vm8, type = 'HC1'))

# Dips analysis

dip1 <- lm(Difference ~ Gender + log(Age) + First_Bad_Race_Pct + log(Altitude) + Some_HS + HS + Some_Uni
           + Associate + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

dip2 <- lm(Difference ~ Gender*log(Age) + First_Bad_Race_Pct + log(Altitude) + Some_HS + HS + Some_Uni
           + Associate + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

dip3 <- lm(Difference ~ Gender + Age + First_Bad_Race_Pct + log(Altitude) + Some_HS + HS + Some_Uni
           + Associate + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

dip4 <- lm(Difference ~ Gender*Age + First_Bad_Race_Pct + log(Altitude) + Some_HS + HS + Some_Uni
           + Associate + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

dip1x <- coeftest(dip1, vcov = vcovCL(dip1, type = 'HC1'))
dip2x <- coeftest(dip2, vcov = vcovCL(dip2, type = 'HC1'))
dip3x <- coeftest(dip3, vcov = vcovCL(dip3, type = 'HC1'))
dip4x <- coeftest(dip4, vcov = vcovCL(dip4, type = 'HC1'))

# Scared away analysis

rp$Scared <- as.integer(rp$Races == 1)

sc1 <- lm(Scared ~ Gender + log(Age) + Some_HS + HS  +Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

sc2 <- lm(Scared ~ Gender*log(Age) + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

sc3 <- lm(Scared ~ Gender + Age + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

sc4 <- lm(Scared ~ Gender*Age + Some_HS + HS + Some_Uni + Associate + Bachelor + Graduate
          + Unemployment_Rate + Median_Household_Income + factor(County) + factor(State), data = rp)

sc1x <- coeftest(sc1, vcov = vcovCL(sc1, type = 'HC1'))
sc2x <- coeftest(sc2, vcov = vcovCL(sc2, type = 'HC1'))
sc3x <- coeftest(sc3, vcov = vcovCL(sc3, type = 'HC1'))
sc4x <- coeftest(sc4, vcov = vcovCL(sc4, type = 'HC1'))

# Viewing the results

stargazer(vm1, vm2, vm3, vm4, vm5, vm6, vm7, vm8, type = 'text', omit.stat = c('f', 'ser'), omit = c('State', 'County'))
stargazer(dip1, dip2, dip3, dip4, type = 'text', omit.stat = c('f', 'ser'), omit = c('State', 'County'))
stargazer(sc1, sc2, sc3, sc4, type = 'text', omit.stat = c('f', 'ser'), omit = c('State', 'County'))

stargazer(vm1x, vm2x, vm3x, vm4x, vm5x, vm6x, vm7x, vm8x, type = 'text', omit = c('State', 'County'))
stargazer(dip1x, dip2x, dip3x, dip4x, type = 'text', omit = c('State', 'County'))
stargazer(sc1x, sc2x, sc3x, sc4x, type = 'text', omit = c('State', 'County'))






