# This script runs regressions for the gender + competition project

# Loading libraries

library(stargazer)
library(sandwich)

# Specifying your username

username <- ''

# Reading in the data

compdata <- read.csv(paste('C:/Users/', username, '/Documents/Data/ultracompetitive/output.csv', sep = ''))

# Running regressions

mod <- lm(Y ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
          + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar), data = compdata)

cm <- vcovHC(mod, type = 'HC0')
rsem <- sqrt(diag(cm))

tmod <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar), data = compdata)

ct <- vcovHC(tmod, type = 'HC0')
rset <- sqrt(diag(ct))

dmod <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar), data = compdata)

cd <- vcovHC(dmod, type = 'HC0')
rsed <- sqrt(diag(cd))

m50k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '50 KM'),])

c50k <- vcovHC(m50k, type = 'HC0')
rse50k <- sqrt(diag(c50k))

m100k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
            + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
            data = compdata[which(compdata$RACE_Distance == '100 KM'),])

c100k <- vcovHC(m100k, type = 'HC0')
rse100k <- sqrt(diag(c100k))

m50m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

c50m <- vcovHC(m50m, type = 'HC0')
rse50m <- sqrt(diag(c50m))

m100m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
            + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
            data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

c100m <- vcovHC(m100m, type = 'HC0')
rse100m <- sqrt(diag(c100m))

m6h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
          + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
          data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

c6h <- vcovHC(m6h, type = 'HC0')
rse6h <- sqrt(diag(c6h))

m12h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

c12h <- vcovHC(m12h, type = 'HC0')
rse12h <- sqrt(diag(c12h))

m24h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

c24h <- vcovHC(m24h, type = 'HC0')
rse24h <- sqrt(diag(c24h))

m48h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY_Percentile
           + Gender_Place_PY_Percentile  + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

c48h <- vcovHC(m48h, type = 'HC0')
rse48h <- sqrt(diag(c48h))

# Viewing and saving the results

write.csv(stargazer(m50k,m100k,m50m,m100m,tmod,m6h,m12h,m24h,m48h,dmod,mod,
                    se = list(rse50k,rse100k,rse50m,rse100m,rset,rse6h,rse12h,rse24h,rse48h,rsed,rsem),
                    omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50k,m100k,m50m,m100m,tmod,m6h,m12h,m24h,m48h,dmod,mod,
                    se = list(rse50k,rse100k,rse50m,rse100m,rset,rse6h,rse12h,rse24h,rse48h,rsed,rsem),
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results.txt', sep = ''), row.names = FALSE)

