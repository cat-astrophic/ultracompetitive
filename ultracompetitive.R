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

ct5 <- vcovHC(t5, type = 'HC0')
rset5 <- sqrt(diag(ct5))

d5 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
         + Experience_Years + Age + Same_County_Competitors + Travel_Distance
         + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
         + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
         + Gender_Place + Age_Place + factor(RACE_Month)
         + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
         + I(Experience_Races^2) + I(Days_Since_Last_Race^2), data = compdata)

cd5 <- vcovHC(d5, type = 'HC0')
rsed5 <- sqrt(diag(cd5))

m50k2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '50 KM'),])

c50k <- vcovHC(m50k2, type = 'HC0')
rse50k <- sqrt(diag(c50k))

m100k2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
            + Experience_Years + Age + Same_County_Competitors + Travel_Distance
            + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
            + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
            + Gender_Place + Age_Place + factor(RACE_Month)
            + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
            + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
            data = compdata[which(compdata$RACE_Distance == '100 KM'),])

c100k <- vcovHC(m100k2, type = 'HC0')
rse100k <- sqrt(diag(c100k))

m50m2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

c50m <- vcovHC(m50m2, type = 'HC0')
rse50m <- sqrt(diag(c50m))

m100m2 <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
            + Experience_Years + Age + Same_County_Competitors + Travel_Distance
            + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
            + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
            + Gender_Place + Age_Place + factor(RACE_Month)
            + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
            + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
            data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

c100m <- vcovHC(m100m2, type = 'HC0')
rse100m <- sqrt(diag(c100m))

m6h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
          + Experience_Years + Age + Same_County_Competitors + Travel_Distance
          + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
          + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
          + Gender_Place + Age_Place + factor(RACE_Month)
          + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
          + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
          data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

c6h <- vcovHC(m6h2, type = 'HC0')
rse6h <- sqrt(diag(c6h))

m12h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

c12h <- vcovHC(m12h2, type = 'HC0')
rse12h <- sqrt(diag(c12h))

m24h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

c24h <- vcovHC(m24h2, type = 'HC0')
rse24h <- sqrt(diag(c24h))

m48h2 <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Experience_Races
           + Experience_Years + Age + Same_County_Competitors + Travel_Distance
           + Days_Since_COVID + Some_College_Ass + College_Degree + Unemployment_Rate
           + Median_Household_Income + Days_Since_Last_Race + In_State + Overall
           + Gender_Place + Age_Place + factor(RACE_Month)
           + factor(RACE_Year) + I(Age^2) + I(Experience_Years^2)
           + I(Experience_Races^2) + I(Days_Since_Last_Race^2),
           data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

c48h <- vcovHC(m48h2, type = 'HC0')
rse48h <- sqrt(diag(c48h))

# Results

stargazer(m50k2,m100k2,m50m2,m100m2,t5, type = 'text', se = list(rse50k,rse100k,rse50m,rse100m),
          omit = c('RACE_Month', 'RACE_Year'), omit.stat = c('f', 'ser'))

stargazer(m6h2,m12h2,m24h2,m48h2,d5, type = 'text', se = list(rse6h,rse12h,rse24h,rse48h),
          omit = c('RACE_Month', 'RACE_Year'), omit.stat = c('f', 'ser'))

