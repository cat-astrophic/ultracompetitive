# This script runs regressions for the gender + competition project

# Loading libraries

library(stargazer)
library(sandwich)
library(progress)
library(sjmisc)

# Specifying your username

username <- ''

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

# Running regressions

mod <- lm(Y ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
          + factor(idvar), data = compdata)

cm <- vcovHC(mod, type = 'HC0')
rsem <- sqrt(diag(cm))

tmod <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata)

ct <- vcovHC(tmod, type = 'HC0')
rset <- sqrt(diag(ct))

dmod <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata)

cd <- vcovHC(dmod, type = 'HC0')
rsed <- sqrt(diag(cd))

m50k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata[which(compdata$RACE_Distance == '50 KM'),])

c50k <- vcovHC(m50k, type = 'HC0')
rse50k <- sqrt(diag(c50k))

m100k <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
            + factor(idvar), data = compdata[which(compdata$RACE_Distance == '100 KM'),])

c100k <- vcovHC(m100k, type = 'HC0')
rse100k <- sqrt(diag(c100k))

m50m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata[which(compdata$RACE_Distance == '50 Miles'),])

c50m <- vcovHC(m50m, type = 'HC0')
rse50m <- sqrt(diag(c50m))

m100m <- lm(Y_time ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
            + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
            + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
            + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
            + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
            + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
            + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
            + factor(idvar), data = compdata[which(compdata$RACE_Distance == '100 Miles'),])

c100m <- vcovHC(m100m, type = 'HC0')
rse100m <- sqrt(diag(c100m))

m6h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
          + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
          + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
          + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
          + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
          + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
          + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
          + factor(idvar), data = compdata[which(compdata$RACE_Distance == '6 Hours'),])

c6h <- vcovHC(m6h, type = 'HC0')
rse6h <- sqrt(diag(c6h))

m12h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata[which(compdata$RACE_Distance == '12 Hours'),])

c12h <- vcovHC(m12h, type = 'HC0')
rse12h <- sqrt(diag(c12h))

m24h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata[which(compdata$RACE_Distance == '24 Hours'),])

c24h <- vcovHC(m24h, type = 'HC0')
rse24h <- sqrt(diag(c24h))

m48h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata[which(compdata$RACE_Distance == '48 Hours'),])

c48h <- vcovHC(m48h, type = 'HC0')
rse48h <- sqrt(diag(c48h))

m72h <- lm(Y_distance ~ factor(Gender) + Competitors + Gender_Pct + Overall_PY
           + Gender_Place_PY + Age_Place_PY + Travel_Distance + Same_County_Competitors
           + In_State + Age + I(Age^2) + Experience_Races + I(Experience_Races^2)
           + Experience_Years + I(Experience_Years^2) + Days_Since_Last_Race
           + I(Days_Since_Last_Race^2) + Altitude + Some_HS + HS + Some_Uni + Associate
           + Bachelor + Graduate + Unemployment_Rate + Median_Household_Income
           + factor(RACE_Month)*factor(RACE_Year)# + Days_Since_COVID + I(Days_Since_COVID^2)
           + factor(idvar), data = compdata[which(compdata$RACE_Distance == '72 Hours'),])

c72h <- vcovHC(m72h, type = 'HC0')
rse72h <- sqrt(diag(c72h))

# Viewing and saving the results

write.csv(stargazer(m50k,m100k,m50m,m100m,tmod,m6h,m12h,m24h,m72h,dmod,mod,
                    se = list(rse50k,rse100k,rse50m,rse100m,rset,rse6h,rse12h,rse24h,rse72h,rsed,rsem),
                    omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_all_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50k,m100k,m50m,m100m,tmod,m6h,m12h,m24h,m72h,dmod,mod,
                    se = list(rse50k,rse100k,rse50m,rse100m,rset,rse6h,rse12h,rse24h,rse72h,rsed,rsem),
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_all.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50k,m100k,m50m,m100m,tmod, se = list(rse50k,rse100k,rse50m,rse100m,rset),
                    omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(m50k,m100k,m50m,m100m,tmod, se = list(rse50k,rse100k,rse50m,rse100m,rset),
                    type = 'text', omit.stat = c('f','ser'), omit = c('RACE_Month', 'RACE_Year', 'idvar')),
          paste('C:/Users/', username, '/Documents/Data/ultracompetitive/results.txt', sep = ''), row.names = FALSE)

