# This script runs regressions for the gender + competition project

# Loading libraries

library(stargazer)
library(sandwich)
library(progress)
library(sjmisc)
library(modelsummary)

# Specifying your username

username <- 'Michael'

# Reading in the data

compdata <- read.csv(paste('C:/Users/', username, '/Documents/Data/ultracompetitive/output.csv', sep = ''))

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
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_noFE_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_noFE.txt', sep = ''), row.names = FALSE)

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
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_stateFE_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_stateFE.txt', sep = ''), row.names = FALSE)

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
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_regionFE_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50kx,m100kx,m50mx,m100mx,tmodx,m6hx,m12hx,m24hx,m48hx,m72hx,dmodx,modx,
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar', 'StateX')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_regionFE.txt', sep = ''), row.names = FALSE)

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

