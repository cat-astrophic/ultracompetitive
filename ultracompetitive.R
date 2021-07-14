# This script does the econometrics for the ultraCOVID project

# Loading libraries

library(stargazer)
library(sandwich)

# Specifying your username

username <- ''

# Reading in the data

compdata <- read.csv(paste('C:/Users/', username, '/Documents/Data/ultracompetitive/output.csv', sep = ''))

# Running regressions

t5 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
         + Experience_Years + Age + Same_County_Competitors + Travel_Distance
         + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
         + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
         + Gender_Place + Age_Place + factor(RACE_Month)
         + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
         + I(Experience_Races^2) + I(Days_Since_Last_Race^2), data = compdata)

d5 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
         + Experience_Years + Age + Same_County_Competitors + Travel_Distance
         + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
         + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
         + Gender_Place + Age_Place + factor(RACE_Month)
         + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
         + I(Experience_Races^2) + I(Days_Since_Last_Race^2), data = compdata)

m50k2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '50 KM'),])

m100k2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
            + Experience_Years + Age + Same_County_Competitors + Travel_Distance
            + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
            + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
            + Gender_Place + Age_Place + factor(RACE_Month)
            + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
            + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
            data = compdata[which(compdata$RACE_Distance == '100 KM'),])

m50m2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

m100m2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
            + Experience_Years + Age + Same_County_Competitors + Travel_Distance
            + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
            + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
            + Gender_Place + Age_Place + factor(RACE_Month)
            + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
            + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
            data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

m6h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
          + Experience_Years + Age + Same_County_Competitors + Travel_Distance
          + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
          + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
          + Gender_Place + Age_Place + factor(RACE_Month)
          + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
          + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
          data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

m12h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

m24h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

m48h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

# Results

stargazer(m50k2,m100k2,m50m2,m100m2,t5, type = 'text',
          omit = c('RACE_Month', 'RACE_Year'), omit.stat = c('f', 'ser'))

stargazer(m6h2,m12h2,m24h2,m48h2,d5, type = 'text',
          omit = c('RACE_Month', 'RACE_Year'), omit.stat = c('f', 'ser'))

