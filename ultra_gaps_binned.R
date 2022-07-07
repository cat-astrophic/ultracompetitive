# This script runs regressions for the gender + competition project

# Loading libraries

library(stargazer)
library(sandwich)
library(progress)
library(sjmisc)
library(ggplot2)
library(glmnet)

# Specifying your username

username <- ''

# Reading in the data

data <- read.csv(paste('C:/Users/', username, '/Documents/Data/ultracompetitive/gap_results/binned_data.csv', sep = ''))

# Remove 2020 data because COVID

data <- data[data$Year < 2020,]

# Subset for events

d50k <- data[data$Event == '50 KM',]
d100k <- data[data$Event == '100 KM',]
d50m <- data[data$Event == '50 Miles',]
d100m <- data[data$Event == '100 Miles',]
d24h <- data[data$Event == '24 Hours',]
d12h <- data[data$Event == '12 Hours',]

# Create data sets for the most recent five years

late.data <- data[data$Year >= 2015,]
d50kx <- late.data[late.data$Event == '50 KM',]
d100kx <- late.data[late.data$Event == '100 KM',]
d50mx <- late.data[late.data$Event == '50 Miles',]
d100mx <- late.data[late.data$Event == '100 Miles',]
d24hx <- late.data[late.data$Event == '24 Hours',]
d12hx <- late.data[late.data$Event == '12 Hours',]

# Running regressions

mod.50k.all <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d50k)
mod.100k.all <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d100k)
mod.50m.all <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d50m)
mod.100m.all <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d100m)
mod.24h.all <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d24h)
mod.12h.all <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d12h)

mod.50k.late <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d50kx)
mod.100k.late <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d100kx)
mod.50m.late <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d50mx)
mod.100m.late <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d100mx)
mod.24h.late <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d24hx)
mod.12h.late <- lm(PD ~ Bin + I(Bin^2) + I(Bin^3) + factor(Year), data = d12hx)

# View the results

stargazer(mod.50k.all,mod.50m.all,mod.100k.all,mod.100m.all,mod.24h.all,mod.12h.all, type = 'text', omit.stat = c('f', 'ser'))
stargazer(mod.50k.late,mod.50m.late,mod.100k.late,mod.100m.late,mod.24h.late,mod.12h.late, type = 'text', omit.stat = c('f', 'ser'))

# Some plots of the cubics

err.all <- predict(mod.50k.all, newdata = d50k, se.fit = TRUE)
d50k$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d50k$fit.all <- err.all$fit
d50k$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d50k, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 50 km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d50k[d50k$Year == 2010,], aes(x = Bin, y = PD, color = '2010')) +
  geom_point(data = d50k[d50k$Year == 2011,], aes(x = Bin, y = PD, color = '2011')) +
  geom_point(data = d50k[d50k$Year == 2012,], aes(x = Bin, y = PD, color = '2012')) +
  geom_point(data = d50k[d50k$Year == 2013,], aes(x = Bin, y = PD, color = '2013')) +
  geom_point(data = d50k[d50k$Year == 2014,], aes(x = Bin, y = PD, color = '2014')) +
  geom_point(data = d50k[d50k$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d50k[d50k$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d50k[d50k$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d50k[d50k$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d50k[d50k$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019'),
                     values = c('2010' = 'pink', '2011' = 'red', '2012' = 'orange', '2013' = 'yellow',
                                '2014' = 'green', '2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.100k.all, newdata = d100k, se.fit = TRUE)
d100k$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d100k$fit.all <- err.all$fit
d100k$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d100k, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 100 km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d100k[d100k$Year == 2010,], aes(x = Bin, y = PD, color = '2010')) +
  geom_point(data = d100k[d100k$Year == 2011,], aes(x = Bin, y = PD, color = '2011')) +
  geom_point(data = d100k[d100k$Year == 2012,], aes(x = Bin, y = PD, color = '2012')) +
  geom_point(data = d100k[d100k$Year == 2013,], aes(x = Bin, y = PD, color = '2013')) +
  geom_point(data = d100k[d100k$Year == 2014,], aes(x = Bin, y = PD, color = '2014')) +
  geom_point(data = d100k[d100k$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d100k[d100k$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d100k[d100k$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d100k[d100k$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d100k[d100k$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019'),
                     values = c('2010' = 'pink', '2011' = 'red', '2012' = 'orange', '2013' = 'yellow',
                                '2014' = 'green', '2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.50m.all, newdata = d50m, se.fit = TRUE)
d50m$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d50m$fit.all <- err.all$fit
d50m$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d50m, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 50 mi') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d50m[d50m$Year == 2010,], aes(x = Bin, y = PD, color = '2010')) +
  geom_point(data = d50m[d50m$Year == 2011,], aes(x = Bin, y = PD, color = '2011')) +
  geom_point(data = d50m[d50m$Year == 2012,], aes(x = Bin, y = PD, color = '2012')) +
  geom_point(data = d50m[d50m$Year == 2013,], aes(x = Bin, y = PD, color = '2013')) +
  geom_point(data = d50m[d50m$Year == 2014,], aes(x = Bin, y = PD, color = '2014')) +
  geom_point(data = d50m[d50m$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d50m[d50m$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d50m[d50m$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d50m[d50m$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d50m[d50m$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019'),
                     values = c('2010' = 'pink', '2011' = 'red', '2012' = 'orange', '2013' = 'yellow',
                                '2014' = 'green', '2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.100m.all, newdata = d100m, se.fit = TRUE)
d100m$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d100m$fit.all <- err.all$fit
d100m$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d100m, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 100 mi') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d100m[d100m$Year == 2010,], aes(x = Bin, y = PD, color = '2010')) +
  geom_point(data = d100m[d100m$Year == 2011,], aes(x = Bin, y = PD, color = '2011')) +
  geom_point(data = d100m[d100m$Year == 2012,], aes(x = Bin, y = PD, color = '2012')) +
  geom_point(data = d100m[d100m$Year == 2013,], aes(x = Bin, y = PD, color = '2013')) +
  geom_point(data = d100m[d100m$Year == 2014,], aes(x = Bin, y = PD, color = '2014')) +
  geom_point(data = d100m[d100m$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d100m[d100m$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d100m[d100m$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d100m[d100m$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d100m[d100m$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019'),
                     values = c('2010' = 'pink', '2011' = 'red', '2012' = 'orange', '2013' = 'yellow',
                                '2014' = 'green', '2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.24h.all, newdata = d24h, se.fit = TRUE)
d24h$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d24h$fit.all <- err.all$fit
d24h$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d24h, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 24 hr') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d24h[d24h$Year == 2010,], aes(x = Bin, y = PD, color = '2010')) +
  geom_point(data = d24h[d24h$Year == 2011,], aes(x = Bin, y = PD, color = '2011')) +
  geom_point(data = d24h[d24h$Year == 2012,], aes(x = Bin, y = PD, color = '2012')) +
  geom_point(data = d24h[d24h$Year == 2013,], aes(x = Bin, y = PD, color = '2013')) +
  geom_point(data = d24h[d24h$Year == 2014,], aes(x = Bin, y = PD, color = '2014')) +
  geom_point(data = d24h[d24h$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d24h[d24h$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d24h[d24h$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d24h[d24h$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d24h[d24h$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019'),
                     values = c('2010' = 'pink', '2011' = 'red', '2012' = 'orange', '2013' = 'yellow',
                                '2014' = 'green', '2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.12h.all, newdata = d12h, se.fit = TRUE)
d12h$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d12h$fit.all <- err.all$fit
d12h$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d12h, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 12 hr') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d12h[d12h$Year == 2010,], aes(x = Bin, y = PD, color = '2010')) +
  geom_point(data = d12h[d12h$Year == 2011,], aes(x = Bin, y = PD, color = '2011')) +
  geom_point(data = d12h[d12h$Year == 2012,], aes(x = Bin, y = PD, color = '2012')) +
  geom_point(data = d12h[d12h$Year == 2013,], aes(x = Bin, y = PD, color = '2013')) +
  geom_point(data = d12h[d12h$Year == 2014,], aes(x = Bin, y = PD, color = '2014')) +
  geom_point(data = d12h[d12h$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d12h[d12h$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d12h[d12h$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d12h[d12h$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d12h[d12h$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019'),
                     values = c('2010' = 'pink', '2011' = 'red', '2012' = 'orange', '2013' = 'yellow',
                                '2014' = 'green', '2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

# Repeat plots of the cubics but restrticted to most recent five years of data

err.all <- predict(mod.50k.late, newdata = d50kx, se.fit = TRUE)
d50kx$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d50kx$fit.all <- err.all$fit
d50kx$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d50kx, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 50 km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d50kx[d50kx$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d50kx[d50kx$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d50kx[d50kx$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d50kx[d50kx$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d50kx[d50kx$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2015', '2016', '2017', '2018', '2019'),
                     values = c('2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.100k.late, newdata = d100kx, se.fit = TRUE)
d100kx$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d100kx$fit.all <- err.all$fit
d100kx$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d100kx, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 100 km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d100kx[d100kx$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d100kx[d100kx$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d100kx[d100kx$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d100kx[d100kx$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d100kx[d100kx$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2015', '2016', '2017', '2018', '2019'),
                     values = c('2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.50m.late, newdata = d50mx, se.fit = TRUE)
d50mx$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d50mx$fit.all <- err.all$fit
d50mx$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d50mx, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 50 mi') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d50mx[d50mx$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d50mx[d50mx$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d50mx[d50mx$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d50mx[d50mx$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d50mx[d50mx$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2015', '2016', '2017', '2018', '2019'),
                     values = c('2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.100m.late, newdata = d100mx, se.fit = TRUE)
d100mx$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d100mx$fit.all <- err.all$fit
d100mx$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d100mx, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 100 mi') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d100mx[d100mx$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d100mx[d100mx$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d100mx[d100mx$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d100mx[d100mx$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d100mx[d100mx$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2015', '2016', '2017', '2018', '2019'),
                     values = c('2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.24h.late, newdata = d24hx, se.fit = TRUE)
d24hx$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d24hx$fit.all <- err.all$fit
d24hx$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d24hx, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 24 hr') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d24hx[d24hx$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d24hx[d24hx$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d24hx[d24hx$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d24hx[d24hx$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d24hx[d24hx$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2015', '2016', '2017', '2018', '2019'),
                     values = c('2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

err.all <- predict(mod.12h.late, newdata = d12hx, se.fit = TRUE)
d12hx$lci.all <- err.all$fit - 1.96 * err.all$se.fit
d12hx$fit.all <- err.all$fit
d12hx$uci.all <- err.all$fit + 1.96 * err.all$se.fit

ggplot(d12hx, aes(x = Bin, y = fit.all)) +
  theme_bw() +
  ggtitle('Percent Difference in Performance by Percentile - 12 hr') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_point(data = d12hx[d12hx$Year == 2015,], aes(x = Bin, y = PD, color = '2015')) +
  geom_point(data = d12hx[d12hx$Year == 2016,], aes(x = Bin, y = PD, color = '2016')) +
  geom_point(data = d12hx[d12hx$Year == 2017,], aes(x = Bin, y = PD, color = '2017')) +
  geom_point(data = d12hx[d12hx$Year == 2018,], aes(x = Bin, y = PD, color = '2018')) +
  geom_point(data = d12hx[d12hx$Year == 2019,], aes(x = Bin, y = PD, color = '2019')) +
  geom_smooth(aes(ymin = lci.all, ymax = uci.all), color = 'black') +
  scale_color_manual(name = 'Years',
                     breaks = c('2015', '2016', '2017', '2018', '2019'),
                     values = c('2015' = 'cyan', '2016' = 'blue', '2017' = 'purple',
                                '2018' = 'brown', '2019' = 'gray'))

# Next....















