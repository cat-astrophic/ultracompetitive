# This script runs regressions for the gender + competition project

# Loading libraries

library(stargazer)
library(sandwich)
library(progress)
library(sjmisc)
library(modelsummary)

# Project directory info

direc <- 'D:/ultracompetitive/'

# Reading in the data

compdata <- read.csv(paste(direc, 'output.csv', sep = ''))

# Drop people who did very poorly in one year or the other in fixed time events

compdata <- compdata[which(is.na(compdata$Y) == FALSE),] # Keeping only those observations with Y values
row.names(compdata) <- NULL # Reset the row indices

rr <- unique(compdata$RACE_Distance)
xx <- c()
cutoffs <- c()

for (r in rr) { # Get list of fixed-time events

  if (str_contains(r, 'Hours') == TRUE) {

    xx <- c(xx, r)

  }

}

for (x in xx) { # Get threshold for bottom 20% of time for each event in xx

  tmp <- compdata[which(compdata$RACE_Distance == x),]
  lower <- quantile(tmp$Distance, c(.2), na.rm = TRUE)
  cutoffs <- c(cutoffs, lower)

}

cutdf <- as.data.frame(cbind(xx, cutoffs))
cutdf$cutoffs <- as.numeric(cutdf$cutoffs)
droplist <- rep(0, dim(compdata)[1])

for (i in 1:dim(compdata)[1]) { # Determine which rows to drop

  print(i) # Visualize progress

  for (j in 1:dim(cutdf)[1]) {

    if (compdata$RACE_Distance[i] == cutdf$xx[j]) {

      if (compdata$Distance[i] < cutdf$cutoffs[j]) {

        droplist[i] <- 1

        if (compdata$Attended_Next_Year_Race[i] == 1) {

          tmp <- compdata[which(compdata$Runner_ID == compdata$Runner_ID[i]),]
          tmp <- tmp[which(tmp$RACE_Name == compdata$RACE_Name[i]),]
          tmp <- tmp[which(tmp$RACE_Year == compdata$RACE_Year[i]+1),]

          droplist[as.integer(row.names(tmp))] <- 1 # Delete next year result as well

        }

      }

    }

  }

}

lower <- quantile(compdata$Y, c(.01), na.rm = TRUE)
upper <- quantile(compdata$Y, c(.99), na.rm = TRUE)

for (i in 1:dim(compdata)[1]) { # Determine which rows to drop

  print(i) # Visualize progress

  for (j in 1:dim(cutdf)[1]) {

    if (compdata$RACE_Distance[i] == cutdf$xx[j]) {

      if (compdata$Y[i] < lower) {

        droplist[i] <- 1

      }

      if (compdata$Y[i] > upper) {

        droplist[i] <- 1

      }

    }

  }

}

# Drop rows highlighted by droplist

compdata$Drop <- droplist
compdata <- compdata[which(compdata$Drop == 0),]
row.names(compdata) <- NULL # Reset the row indices

# Drop gender imbalanced races

compdata <- compdata[which(compdata$Gender_Pct > 20),]
compdata <- compdata[which(compdata$Gender_Pct < 80),]

# Cleaning state codes

errors <- c("\"CA", "Wi", "Il", "il", "Nj", "Ks", "\"IL", "Ga", "ca", "Pa", "Fl", "az", "mi", "wa", "wi", "Ca", "Co", "\"GA", "Al", "Wa")
replacements <- c("CA", "WI", "IL", "IL", "NJ", "KS", "IL", "GA", "CA", "PA", "FL", "AZ", "MI", "WA", "WI", "CA", "CO", "GA", "AL", "WA")

sc <- c()

for (s in compdata$State) {if (s %in% errors) {sc <- c(sc,replacements[which(errors == s)])} else {sc <- c(sc,s)}}

compdata$StateX <- sc

# Adding census regions

states <- unique(compdata$StateX)
regions <- c(1,1,1,1,1,2,3,2,4,6,3,2,5,5,4,4,2,2,4,6,2,4,5,5,3,5,5,5,6,3,2,3,3,5,2,4,4,5,6,6,2,3,5,6,3,6,1,1,5,1,5)

rr <- c()

for (s in compdata$StateX) {rr <- c(rr,regions[which(states == s)])}

compdata$Region <- rr

# Running regressions with no state or region level fixed effects

mod <- lm(Y ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
          data = compdata)

modx <- coeftest(mod, vcov = vcovCL, cluster = ~idvar)

tmod <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata)

tmodx <- coeftest(tmod, vcov = vcovCL, cluster = ~idvar)

dmod <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata)

dmodx <- coeftest(dmod, vcov = vcovCL, cluster = ~idvar)

m50k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '50 KM'),])

m50kx <- coeftest(m50k, vcov = vcovCL, cluster = ~idvar)

m100k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
            data = compdata[which(compdata$RACE_Distance == '100 KM'),])

m100kx <- coeftest(m100k, vcov = vcovCL, cluster = ~idvar)

m50m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

m50mx <- coeftest(m50m, vcov = vcovCL, cluster = ~idvar)

m100m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
            data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

m100mx <- coeftest(m100m, vcov = vcovCL, cluster = ~idvar)

m6h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
          data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

m6hx <- coeftest(m6h, vcov = vcovCL, cluster = ~idvar)

m12h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

m12hx <- coeftest(m12h, vcov = vcovCL, cluster = ~idvar)

m24h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

m24hx <- coeftest(m24h, vcov = vcovCL, cluster = ~idvar)

m48h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

m48hx <- coeftest(m48h, vcov = vcovCL, cluster = ~idvar)

m72h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar),
           data = compdata[which(compdata$RACE_Distance == '72 Hours'),])

m72hx <- coeftest(m72h, vcov = vcovCL, cluster = ~idvar)

# Viewing and saving the results

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste(direc, 'results_noFE_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste(direc, 'results_noFE.txt', sep = ''), row.names = FALSE)

# Running regressions with state fixed effects

mod <- lm(Y ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
          data = compdata)

modx <- coeftest(mod, vcov = vcovCL, cluster = ~idvar)

tmod <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata)

tmodx <- coeftest(tmod, vcov = vcovCL, cluster = ~idvar)

dmod <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata)

dmodx <- coeftest(dmod, vcov = vcovCL, cluster = ~idvar)

m50k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata[which(compdata$RACE_Distance == '50 KM'),])

m50kx <- coeftest(m50k, vcov = vcovCL, cluster = ~idvar)

m100k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
            data = compdata[which(compdata$RACE_Distance == '100 KM'),])

m100kx <- coeftest(m100k, vcov = vcovCL, cluster = ~idvar)

m50m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

m50mx <- coeftest(m50m, vcov = vcovCL, cluster = ~idvar)

m100m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
            data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

m100mx <- coeftest(m100m, vcov = vcovCL, cluster = ~idvar)

m6h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
          data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

m6hx <- coeftest(m6h, vcov = vcovCL, cluster = ~idvar)

m12h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

m12hx <- coeftest(m12h, vcov = vcovCL, cluster = ~idvar)

m24h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

m24hx <- coeftest(m24h, vcov = vcovCL, cluster = ~idvar)

m48h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

m48hx <- coeftest(m48h, vcov = vcovCL, cluster = ~idvar)

m72h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(StateX),
           data = compdata[which(compdata$RACE_Distance == '72 Hours'),])

m72hx <- coeftest(m72h, vcov = vcovCL, cluster = ~idvar)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste(direc, 'results_stateFE_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste(direc, 'results_stateFE.txt', sep = ''), row.names = FALSE)

# Running regressions with census region fixed effects

mod <- lm(Y ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
          data = compdata)

modx <- coeftest(mod, vcov = vcovCL, cluster = ~idvar)

tmod <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata)

tmodx <- coeftest(tmod, vcov = vcovCL, cluster = ~idvar)

dmod <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata)

dmodx <- coeftest(dmod, vcov = vcovCL, cluster = ~idvar)

m50k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata[which(compdata$RACE_Distance == '50 KM'),])

m50kx <- coeftest(m50k, vcov = vcovCL, cluster = ~idvar)

m100k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
            data = compdata[which(compdata$RACE_Distance == '100 KM'),])

m100kx <- coeftest(m100k, vcov = vcovCL, cluster = ~idvar)

m50m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

m50mx <- coeftest(m50m, vcov = vcovCL, cluster = ~idvar)

m100m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
            data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

m100mx <- coeftest(m100m, vcov = vcovCL, cluster = ~idvar)

m6h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
          data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

m6hx <- coeftest(m6h, vcov = vcovCL, cluster = ~idvar)

m12h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

m12hx <- coeftest(m12h, vcov = vcovCL, cluster = ~idvar)

m24h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

m24hx <- coeftest(m24h, vcov = vcovCL, cluster = ~idvar)

m48h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

m48hx <- coeftest(m48h, vcov = vcovCL, cluster = ~idvar)

m72h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year) + factor(idvar) + factor(Region),
           data = compdata[which(compdata$RACE_Distance == '72 Hours'),])

m72hx <- coeftest(m72h, vcov = vcovCL, cluster = ~idvar)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste(direc, 'results_regionFE_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste(direc, 'results_regionFE.txt', sep = ''), row.names = FALSE)

# Creating summary statistics

compdata$FFF <- as.integer(compdata$Gender == 'F')

keepers <- c('Y', 'Y_distance', 'Y_time', 'FFF', 'Competitors', 'Same_County_Competitors',
             'Overall', 'Gender_Place', 'Age_Place', 'Travel_Distance', 'In_State', 'Age',
             'Experience_Races', 'Experience_Years', 'Days_Since_Last_Race', 'Altitude')

new_names <- c('Change in Performance', 'Change in Performance - Time Based Events', 'Change in Performance - Distance Based Events',
               'Female', 'Number of Competitors', 'Number of Same County Competitors', 'Overall Place', 'Gender Place',
               'Age Group Place', 'Travel Distance', 'In State', 'Age', 'Previous Races',
               'Experience in Years', 'Days Since Last Race', 'Altitude of Event')

sumdata <- compdata[, keepers]
names(sumdata) <- new_names

png(paste(direc, 'figures/sum_stats.png', sep = ''))
datasummary_skim(sumdata, fmt = '%.3f')
dev.off()

# A simulation for the paper using percentiles

binner <- function (veccy) {

  outvec <- c()

  for (i in 1:100) {

    outvec <- c(outvec, mean(veccy[(100*(i-1)+1):(i*100)]))

  }

  return(outvec)

}

set.seed(42069)

f.data <- c()
m.data <- c()
sim.data <- c()

for (it in 1:1000) {

  print(it)

  DM <- rnorm(10000, mean = 100, sd = 10)
  DF <- rnorm(10000, mean = 105, sd = 10)

  sorted.dm <- sort(DM, decreasing = FALSE)
  sorted.df <- sort(DF, decreasing = FALSE)

  f.data <- c(f.data, DF)
  m.data <- c(m.data, DM)

  pct.sorted.df <- binner(sorted.df)
  pct.sorted.dm <- binner(sorted.dm)

  pct.diff <- 100*(pct.sorted.df - pct.sorted.dm) / pct.sorted.dm
  sim.data <- cbind(sim.data, pct.diff)

}

plot.df <- as.data.frame(cbind(1:100, rowMeans(sim.data), apply(sim.data, 1, sd)))
names(plot.df) <- c('X', 'Mean', 'SD')

ggplot(data = plot.df, aes(x = X, y = Mean)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Percentile') +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'lightgray') +
  geom_hline(yintercept = 5, color = 'black', linetype = 3) +
  geom_line(aes(y = Mean), size = 1, alpha = 1, color = 'black') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Sample Ratio', breaks = c('1', '2', '3', '4', '5', '10'),
                     values = c('1' = 'red', '2' = 'orange', '3' = 'yellow', '4' = 'green', '5' = 'blue', '10' = 'purple')) +
  scale_x_continuous(breaks = c(1, seq(10, 100, 10)), labels = c(1, seq(10, 100, 10))) +
  ylim(c(0,10))

ggplot(data = plot.df, aes(x = X, y = Mean)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Percentile') +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'orange') +
  geom_hline(yintercept = 5, color = 'black', linetype = 3) +
  geom_line(aes(y = Mean), size = 1, alpha = 1, color = 'red4') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Sample Ratio', breaks = c('1', '2', '3', '4', '5', '10'),
                     values = c('1' = 'red', '2' = 'orange', '3' = 'yellow', '4' = 'green', '5' = 'blue', '10' = 'purple')) +
  scale_x_continuous(breaks = c(1, seq(10, 100, 10)), labels = c(1, seq(10, 100, 10))) +
  ylim(c(0,10))

sim.df <- as.data.frame(c(f.data, m.data))
sim.df$Gender <- c(rep('F', length(f.data)), rep('M', length(m.data)))
names(sim.df) <- c('Values', 'Gender')

ggplot(data = sim.df, aes(Values, fill = Gender)) +
  geom_density(alpha = 0.2) +
  labs(title = 'Comparison of Simulated Values by Gender', x = 'Simualted Value', y = 'Frequency') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('F' = 'orange', 'M' = 'red4')) +
  geom_vline(xintercept = mean(sim.df[which(sim.df$Gender == 'F'),]$Values), linetype = 3, color = 'black') +
  geom_vline(xintercept = mean(sim.df[which(sim.df$Gender == 'M'),]$Values), linetype = 3, color = 'black') +
  #scale_x_continuous(breaks = seq(40, 160, 20), labels = seq(40, 160, 20))
  scale_x_continuous(breaks = c(40, 60, 80, 100, 105, 120, 140, 160), labels = c(40, 60, 80, 100, 105, 120, 140, 160))

# A simulation for the paper using place with a basic sampling component

f.data2 <- c()
m.data2 <- c()
sim.data2 <- c()

for (it in 1:1000) {

  print(it)

  DM2 <- rnorm(10000, mean = 100, sd = 10)
  DF2 <- rnorm(10000, mean = 105, sd = 10)

  DMX2 <- sample(DM2, 100)
  DFX2 <- sample(DF2, 100)

  sorted.dm2 <- sort(DMX2, decreasing = FALSE)
  sorted.df2 <- sort(DFX2, decreasing = FALSE)

  f.data2 <- c(f.data2, DFX2)
  m.data2 <- c(m.data2, DMX2)

  pct.diff <- 100*(sorted.df2 - sorted.dm2) / sorted.dm2
  sim.data2 <- cbind(sim.data2, pct.diff)

}

plot.df2 <- as.data.frame(cbind(1:100, rowMeans(sim.data2), apply(sim.data2, 1, sd)))
names(plot.df2) <- c('X', 'Mean', 'SD')

ggplot(data = plot.df2, aes(x = X, y = Mean)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Percentile') +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'lightgray') +
  geom_hline(yintercept = 5, color = 'black', linetype = 3) +
  geom_line(aes(y = Mean), size = 1, alpha = 1, color = 'black') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Sample Ratio', breaks = c('1', '2', '3', '4', '5', '10'),
                     values = c('1' = 'red', '2' = 'orange', '3' = 'yellow', '4' = 'green', '5' = 'blue', '10' = 'purple')) +
  scale_x_continuous(breaks = c(1, seq(10, 100, 10)), labels = c(1, seq(10, 100, 10)))

ggplot(data = plot.df2, aes(x = X, y = Mean)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Percentile') +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'orange') +
  geom_hline(yintercept = 5, color = 'black', linetype = 3) +
  geom_line(aes(y = Mean), size = 1, alpha = 1, color = 'red4') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Sample Ratio', breaks = c('1', '2', '3', '4', '5', '10'),
                     values = c('1' = 'red', '2' = 'orange', '3' = 'yellow', '4' = 'green', '5' = 'blue', '10' = 'purple')) +
  scale_x_continuous(breaks = c(1, seq(10, 100, 10)), labels = c(1, seq(10, 100, 10)))

sim.df2 <- as.data.frame(c(f.data2, m.data2))
sim.df2$Gender <- c(rep('F', length(f.data2)), rep('M', length(m.data2)))
names(sim.df2) <- c('Values', 'Gender')

ggplot(data = sim.df2, aes(Values, fill = Gender)) +
  geom_density(alpha = 0.2) +
  labs(title = 'Comparison of Simulated Values by Gender', x = 'Simualted Value', y = 'Frequency') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('F' = 'orange', 'M' = 'red4')) +
  geom_vline(xintercept = mean(sim.df[which(sim.df$Gender == 'F'),]$Values), linetype = 3, color = 'black') +
  geom_vline(xintercept = mean(sim.df[which(sim.df$Gender == 'M'),]$Values), linetype = 3, color = 'black') +
  #scale_x_continuous(breaks = seq(40, 160, 20), labels = seq(40, 160, 20))
  scale_x_continuous(breaks = c(40, 60, 80, 100, 105, 120, 140, 160), labels = c(40, 60, 80, 100, 105, 120, 140, 160))

# A simulation for the paper using place with a more accurate sampling component

f.data3 <- c()
f.data3.2 <- c()
f.data3.5 <- c()
f.data3.10 <- c()
f.data3.20 <- c()

m.data3 <- c()

sim.data3 <- c()
sim.data3.2 <- c()
sim.data3.5 <- c()
sim.data3.10 <- c()
sim.data3.20 <- c()

for (it in 1:1000) {

  print(it)

  DM3 <- rnorm(10000, mean = 100, sd = 10)
  DF3 <- rnorm(10000, mean = 105, sd = 10)

  DM3 <- sort(DM3, decreasing = FALSE)
  DF3 <- sort(DF3, decreasing = FALSE)

  DMX3 <- DM3[1:500]
  DFX3.2 <- sample(DF3[1:1000], 500)
  DFX3.5 <- sample(DF3[1:2500], 500)
  DFX3.10 <- sample(DF3[1:5000], 500)
  DFX3.20 <- sample(DF3[1:10000], 500)
  DFX3 <- DF3[1:500]

  sorted.dm3 <- sort(DMX3, decreasing = FALSE)
  sorted.df3 <- sort(DFX3, decreasing = FALSE)
  sorted.df3.2 <- sort(DFX3.2, decreasing = FALSE)
  sorted.df3.5 <- sort(DFX3.5, decreasing = FALSE)
  sorted.df3.10 <- sort(DFX3.10, decreasing = FALSE)
  sorted.df3.20 <- sort(DFX3.20, decreasing = FALSE)

  f.data3 <- c(f.data3, DFX3)
  f.data3.2 <- c(f.data3.2, DFX3.2)
  f.data3.5 <- c(f.data3.5, DFX3.5)
  f.data3.10 <- c(f.data3.10, DFX3.10)
  f.data3.20 <- c(f.data3.20, DFX3.20)
  m.data3 <- c(m.data3, DMX3)

  pct.diff <- 100*(sorted.df3 - sorted.dm3) / sorted.dm3
  pct.diff.2 <- 100*(sorted.df3.2 - sorted.dm3) / sorted.dm3
  pct.diff.5 <- 100*(sorted.df3.5 - sorted.dm3) / sorted.dm3
  pct.diff.10 <- 100*(sorted.df3.10 - sorted.dm3) / sorted.dm3
  pct.diff.20 <- 100*(sorted.df3.20 - sorted.dm3) / sorted.dm3

  sim.data3 <- cbind(sim.data3, pct.diff)
  sim.data3.2 <- cbind(sim.data3.2, pct.diff.2)
  sim.data3.5 <- cbind(sim.data3.5, pct.diff.5)
  sim.data3.10 <- cbind(sim.data3.10, pct.diff.10)
  sim.data3.20 <- cbind(sim.data3.20, pct.diff.20)

}

plot.df3 <- as.data.frame(cbind(1:500, rowMeans(sim.data3), apply(sim.data3, 1, sd)))
plot.df3.2 <- as.data.frame(cbind(1:500, rowMeans(sim.data3.2), apply(sim.data3.2, 1, sd)))
plot.df3.5 <- as.data.frame(cbind(1:500, rowMeans(sim.data3.5), apply(sim.data3.5, 1, sd)))
plot.df3.10 <- as.data.frame(cbind(1:500, rowMeans(sim.data3.10), apply(sim.data3.10, 1, sd)))
plot.df3.20 <- as.data.frame(cbind(1:500, rowMeans(sim.data3.20), apply(sim.data3.20, 1, sd)))

names(plot.df3) <- c('X', 'Mean', 'SD')
names(plot.df3.2) <- c('X', 'Mean', 'SD')
names(plot.df3.5) <- c('X', 'Mean', 'SD')
names(plot.df3.10) <- c('X', 'Mean', 'SD')
names(plot.df3.20) <- c('X', 'Mean', 'SD')

combo.df <- as.data.frame(cbind(plot.df3$X, plot.df3$Mean, plot.df3.2$Mean, plot.df3.5$Mean, plot.df3.10$Mean, plot.df3.20$Mean, plot.df3$SD, plot.df3.2$SD, plot.df3.5$SD, plot.df3.10$SD, plot.df3.20$SD))
colnames(combo.df) <- c('X', 'R1', 'R2', 'R5', 'R10', 'R20', 'SD1', 'SD2', 'SD5', 'SD10', 'SD20')

ggplot(data = combo.df, aes(x = X, y = R2)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Place') +
  ylab('Percent Difference') +
  xlab('Place') +
  geom_hline(yintercept = 5, color = 'black', linetype = 1) +
  geom_line(aes(y = R1, col = '1'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R2, col = '2'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R5, col = '5'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R10, col = '10'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R20, col = '20'), size = 1, alpha = 0.1) +
  geom_ribbon(aes(ymin = R1 - 2*SD1, ymax = R1 + 2*SD1), size = 1, alpha = .333, fill = 'red4') +
  geom_ribbon(aes(ymin = R2 - 2*SD2, ymax = R2 + 2*SD2), size = 1, alpha = .333, fill = 'orange') +
  geom_ribbon(aes(ymin = R5 - 2*SD5, ymax = R5 + 2*SD5), size = 1, alpha = .333, fill = 'green') +
  geom_ribbon(aes(ymin = R10 - 2*SD10, ymax = R10 + 2*SD10), size = 1, alpha = .333, fill = 'blue') +
  geom_ribbon(aes(ymin = R20 - 2*SD20, ymax = R20 + 2*SD20), size = 1, alpha = .333, fill = 'purple') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Sample Ratio', breaks = c('1', '2', '5', '10', '20'),
                     values = c('1' = 'red4', '2' = 'orange', '5' = 'green', '10' = 'blue', '20' = 'purple'))
scale_x_continuous(breaks = c(1, seq(50, 500, 50)), labels = c(1, seq(50, 500, 50)))

