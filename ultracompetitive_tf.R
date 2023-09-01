# This script analyzes the world athletics data

# Loading libraries

library(dplyr)
library(ggplot2)
library(stargazer)

# Project directory info

direc <- 'D:/ultracompetitive/'

# Reading in the data from world athletics

wa <- read.csv(paste0(direc, 'international_data/iaaf/iaaf.csv'))

# Adding a time column

wa$Time <- wa$Year - 2000

# Subset for the US

us <- wa %>% filter(Nation == 'USA')

# Subset by event

w1 <- wa %>% filter(Event == '100-metres')
w2 <- wa %>% filter(Event == '200-metres')
w4 <- wa %>% filter(Event == '400-metres')
w8 <- wa %>% filter(Event == '800-metres')
w15 <- wa %>% filter(Event == '1500-metres')
w5 <- wa %>% filter(Event == '5000-metres')
w10 <- wa %>% filter(Event == '10000-metres')
wmar <- wa %>% filter(Event == 'marathon')

u1 <- us %>% filter(Event == '100-metres')
u2 <- us %>% filter(Event == '200-metres')
u4 <- us %>% filter(Event == '400-metres')
u8 <- us %>% filter(Event == '800-metres')
u15 <- us %>% filter(Event == '1500-metres')
u5 <- us %>% filter(Event == '5000-metres')
u10 <- us %>% filter(Event == '10000-metres')
umar <- us %>% filter(Event == 'marathon')

# Add a runner-rank column

runner_ranker <- function (df, g, r, e, y) {

  tmp <- df %>% filter(Gender == g) %>% filter(Event == e) %>% filter(Year == y)

  return(which(tmp$Runner == r)[1])

}

rr.w1 <- c()
rr.w2 <- c()
rr.w4 <- c()
rr.w8 <- c()
rr.w15 <- c()
rr.w5 <- c()
rr.w10 <- c()
rr.wmar <- c()

rr.u1 <- c()
rr.u2 <- c()
rr.u4 <- c()
rr.u8 <- c()
rr.u15 <- c()
rr.u5 <- c()
rr.u10 <- c()
rr.umar <- c()

for (i in 1:dim(w1)[1]) {

  print(paste('World :: 100m :: Runner', i, 'of', dim(w1)[1], sep = ' '))
  rr.w1 <- c(rr.w1, runner_ranker(w1, w1$Gender[i], w1$Runner[i], w1$Event[i], w1$Year[i]))

}

for (i in 1:dim(w2)[1]) {

  print(paste('World :: 200m :: Runner', i, 'of', dim(w2)[1], sep = ' '))
  rr.w2 <- c(rr.w2, runner_ranker(w2, w2$Gender[i], w2$Runner[i], w2$Event[i], w2$Year[i]))

}

for (i in 1:dim(w4)[1]) {

  print(paste('World :: 400m :: Runner', i, 'of', dim(w4)[1], sep = ' '))
  rr.w4 <- c(rr.w4, runner_ranker(w4, w4$Gender[i], w4$Runner[i], w4$Event[i], w4$Year[i]))

}

for (i in 1:dim(w8)[1]) {

  print(paste('World :: 800m :: Runner', i, 'of', dim(w8)[1], sep = ' '))
  rr.w8 <- c(rr.w8, runner_ranker(w8, w8$Gender[i], w8$Runner[i], w8$Event[i], w8$Year[i]))

}

for (i in 1:dim(w15)[1]) {

  print(paste('World :: 1,500m :: Runner', i, 'of', dim(w15)[1], sep = ' '))
  rr.w15 <- c(rr.w15, runner_ranker(w15, w15$Gender[i], w15$Runner[i], w15$Event[i], w15$Year[i]))

}

for (i in 1:dim(w5)[1]) {

  print(paste('World :: 5,000m :: Runner', i, 'of', dim(w5)[1], sep = ' '))
  rr.w5 <- c(rr.w5, runner_ranker(w5, w5$Gender[i], w5$Runner[i], w5$Event[i], w5$Year[i]))

}

for (i in 1:dim(w10)[1]) {

  print(paste('World :: 10,000m :: Runner', i, 'of', dim(w10)[1], sep = ' '))
  rr.w10 <- c(rr.w10, runner_ranker(w10, w10$Gender[i], w10$Runner[i], w10$Event[i], w10$Year[i]))

}

for (i in 1:dim(wmar)[1]) {

  print(paste('World :: Marathon :: Runner', i, 'of', dim(wmar)[1], sep = ' '))
  rr.wmar <- c(rr.wmar, runner_ranker(wmar, wmar$Gender[i], wmar$Runner[i], wmar$Event[i], wmar$Year[i]))

}

for (i in 1:dim(u1)[1]) {

  print(paste('US :: 100m :: Runner', i, 'of', dim(u1)[1], sep = ' '))
  rr.u1 <- c(rr.u1, runner_ranker(u1, u1$Gender[i], u1$Runner[i], u1$Event[i], u1$Year[i]))

}

for (i in 1:dim(u2)[1]) {

  print(paste('US :: 200m :: Runner', i, 'of', dim(u2)[1], sep = ' '))
  rr.u2 <- c(rr.u2, runner_ranker(u2, u2$Gender[i], u2$Runner[i], u2$Event[i], u2$Year[i]))

}

for (i in 1:dim(u4)[1]) {

  print(paste('US :: 400m :: Runner', i, 'of', dim(u4)[1], sep = ' '))
  rr.u4 <- c(rr.u4, runner_ranker(u4, u4$Gender[i], u4$Runner[i], u4$Event[i], u4$Year[i]))

}

for (i in 1:dim(u8)[1]) {

  print(paste('US :: 800m :: Runner', i, 'of', dim(u8)[1], sep = ' '))
  rr.u8 <- c(rr.u8, runner_ranker(u8, u8$Gender[i], u8$Runner[i], u8$Event[i], u8$Year[i]))

}

for (i in 1:dim(u15)[1]) {

  print(paste('US :: 1,500m :: Runner', i, 'of', dim(u15)[1], sep = ' '))
  rr.u15 <- c(rr.u15, runner_ranker(u15, u15$Gender[i], u15$Runner[i], u15$Event[i], u15$Year[i]))

}

for (i in 1:dim(u5)[1]) {

  print(paste('US :: 5,000m :: Runner', i, 'of', dim(u5)[1], sep = ' '))
  rr.u5 <- c(rr.u5, runner_ranker(u5, u5$Gender[i], u5$Runner[i], u5$Event[i], u5$Year[i]))

}

for (i in 1:dim(u10)[1]) {

  print(paste('US :: 10,000m :: Runner', i, 'of', dim(u10)[1], sep = ' '))
  rr.u10 <- c(rr.u10, runner_ranker(u10, u10$Gender[i], u10$Runner[i], u10$Event[i], u10$Year[i]))

}

for (i in 1:dim(umar)[1]) {

  print(paste('US :: Marathon :: Runner', i, 'of', dim(umar)[1], sep = ' '))
  rr.umar <- c(rr.umar, runner_ranker(umar, umar$Gender[i], umar$Runner[i], umar$Event[i], umar$Year[i]))

}

w1$Rank2 <- rr.w1
w2$Rank2 <- rr.w2
w4$Rank2 <- rr.w4
w8$Rank2 <- rr.w8
w15$Rank2 <- rr.w15
w5$Rank2 <- rr.w5
w10$Rank2 <- rr.w10
wmar$Rank2 <- rr.wmar

u1$Rank2 <- rr.u1
u2$Rank2 <- rr.u2
u4$Rank2 <- rr.u4
u8$Rank2 <- rr.u8
u15$Rank2 <- rr.u15
u5$Rank2 <- rr.u5
u10$Rank2 <- rr.u10
umar$Rank2 <- rr.umar

# Making performance ranking data within the US

r31 <- c()
r32 <- c()
r34 <- c()
r38 <- c()
r315 <- c()
r35 <- c()
r310 <- c()
r3mar <- c()


for (i in 1:dim(u1)[1]) {

  print(paste('US :: 100m :: Runner', i, 'of', dim(u1)[1], sep = ' '))
  tmp <- u1 %>% filter(Year == u1$Year[i]) %>% filter(Gender == u1$Gender[i]) %>% filter(Event == '100-metres')
  r31 <- c(r31, which(tmp$Mark == u1$Mark[i])[1])

}

for (i in 1:dim(u2)[1]) {

  print(paste('US :: 200m :: Runner', i, 'of', dim(u2)[1], sep = ' '))
  tmp <- u2 %>% filter(Year == u2$Year[i]) %>% filter(Gender == u2$Gender[i]) %>% filter(Event == '200-metres')
  r32 <- c(r32, which(tmp$Mark == u2$Mark[i])[1])

}

for (i in 1:dim(u4)[1]) {

  print(paste('US :: 400m :: Runner', i, 'of', dim(u4)[1], sep = ' '))
  tmp <- u4 %>% filter(Year == u4$Year[i]) %>% filter(Gender == u4$Gender[i]) %>% filter(Event == '400-metres')
  r34 <- c(r34, which(tmp$Mark == u4$Mark[i])[1])

}

for (i in 1:dim(u8)[1]) {

  print(paste('US :: 800m :: Runner', i, 'of', dim(u8)[1], sep = ' '))
  tmp <- u8 %>% filter(Year == u8$Year[i]) %>% filter(Gender == u8$Gender[i]) %>% filter(Event == '800-metres')
  r38 <- c(r38, which(tmp$Mark == u8$Mark[i])[1])

}

for (i in 1:dim(u15)[1]) {

  print(paste('US :: 1,500m :: Runner', i, 'of', dim(u15)[1], sep = ' '))
  tmp <- u15 %>% filter(Year == u15$Year[i]) %>% filter(Gender == u15$Gender[i]) %>% filter(Event == '1500-metres')
  r315 <- c(r315, which(tmp$Mark == u15$Mark[i])[1])

}

for (i in 1:dim(u5)[1]) {

  print(paste('US :: 5,000m :: Runner', i, 'of', dim(u5)[1], sep = ' '))
  tmp <- u5 %>% filter(Year == u5$Year[i]) %>% filter(Gender == u5$Gender[i]) %>% filter(Event == '5000-metres')
  r35 <- c(r35, which(tmp$Mark == u5$Mark[i])[1])

}

for (i in 1:dim(u10)[1]) {

  print(paste('US :: 10,000m :: Runner', i, 'of', dim(u10)[1], sep = ' '))
  tmp <- u10 %>% filter(Year == u10$Year[i]) %>% filter(Gender == u10$Gender[i]) %>% filter(Event == '10000-metres')
  r310 <- c(r310, which(tmp$Mark == u10$Mark[i])[1])

}

for (i in 1:dim(umar)[1]) {

  print(paste('US :: Marathon :: Runner', i, 'of', dim(umar)[1], sep = ' '))
  tmp <- umar %>% filter(Year == umar$Year[i]) %>% filter(Gender == umar$Gender[i]) %>% filter(Event == 'marathon')
  r3mar <- c(r3mar, which(tmp$Mark == umar$Mark[i])[1])

}

u1$Rank3 <- r31
u2$Rank3 <- r32
u4$Rank3 <- r34
u8$Rank3 <- r38
u15$Rank3 <- r315
u5$Rank3 <- r35
u10$Rank3 <- r310
umar$Rank3 <- r3mar

# Making big dataframes

women.wa <- rbind(w1[which(w1$Gender == 'women'),], w2[which(w2$Gender == 'women'),], w4[which(w4$Gender == 'women'),], w8[which(w8$Gender == 'women'),], w15[which(w15$Gender == 'women'),], w5[which(w5$Gender == 'women'),], w10[which(w10$Gender == 'women'),], wmar[which(wmar$Gender == 'women'),])
women.us <- rbind(u1[which(u1$Gender == 'women'),], u2[which(u2$Gender == 'women'),], u4[which(u4$Gender == 'women'),], u8[which(u8$Gender == 'women'),], u15[which(u15$Gender == 'women'),], u5[which(u5$Gender == 'women'),], u10[which(u10$Gender == 'women'),], umar[which(umar$Gender == 'women'),])

men.wa <- rbind(w1[which(w1$Gender == 'men'),], w2[which(w2$Gender == 'men'),], w4[which(w4$Gender == 'men'),], w8[which(w8$Gender == 'men'),], w15[which(w15$Gender == 'men'),], w5[which(w5$Gender == 'men'),], w10[which(w10$Gender == 'men'),], wmar[which(wmar$Gender == 'men'),])
men.us <- rbind(u1[which(u1$Gender == 'men'),], u2[which(u2$Gender == 'men'),], u4[which(u4$Gender == 'men'),], u8[which(u8$Gender == 'men'),], u15[which(u15$Gender == 'men'),], u5[which(u5$Gender == 'men'),], u10[which(u10$Gender == 'men'),], umar[which(umar$Gender == 'men'),])

# Making gendered dataframes

w1w <- w1 %>% filter(Gender == 'women')
w2w <- w2 %>% filter(Gender == 'women')
w4w <- w4 %>% filter(Gender == 'women')
w8w <- w8 %>% filter(Gender == 'women')
w15w <- w15 %>% filter(Gender == 'women')
w5w <- w5 %>% filter(Gender == 'women')
w10w <- w10 %>% filter(Gender == 'women')
wmarw <- wmar %>% filter(Gender == 'women')

w1m <- w1 %>% filter(Gender == 'men')
w2m <- w2 %>% filter(Gender == 'men')
w4m <- w4 %>% filter(Gender == 'men')
w8m <- w8 %>% filter(Gender == 'men')
w15m <- w15 %>% filter(Gender == 'men')
w5m <- w5 %>% filter(Gender == 'men')
w10m <- w10 %>% filter(Gender == 'men')
wmarm <- wmar %>% filter(Gender == 'men')

u1w <- u1 %>% filter(Gender == 'women')
u2w <- u2 %>% filter(Gender == 'women')
u4w <- u4 %>% filter(Gender == 'women')
u8w <- u8 %>% filter(Gender == 'women')
u15w <- u15 %>% filter(Gender == 'women')
u5w <- u5 %>% filter(Gender == 'women')
u10w <- u10 %>% filter(Gender == 'women')
umarw <- umar %>% filter(Gender == 'women')

u1m <- u1 %>% filter(Gender == 'men')
u2m <- u2 %>% filter(Gender == 'men')
u4m <- u4 %>% filter(Gender == 'men')
u8m <- u8 %>% filter(Gender == 'men')
u15m <- u15 %>% filter(Gender == 'men')
u5m <- u5 %>% filter(Gender == 'men')
u10m <- u10 %>% filter(Gender == 'men')
umarm <- umar %>% filter(Gender == 'men')

# Making percent difference dataframes

pd.100 <- c()
pd.200 <- c()
pd.400 <- c()
pd.800 <- c()
pd.1500 <- c()
pd.5000 <- c()
pd.10000 <- c()
pd.mar <- c()

upd.100 <- c()
upd.200 <- c()
upd.400 <- c()
upd.800 <- c()
upd.1500 <- c()
upd.5000 <- c()
upd.10000 <- c()
upd.mar <- c()

for (y in 2001:2022) {

  xw1w <- w1w[which(w1w$Year == y),]
  xw2w <- w2w[which(w2w$Year == y),]
  xw4w <- w4w[which(w4w$Year == y),]
  xw8w <- w8w[which(w8w$Year == y),]
  xw15w <-w15w[which(w15w$Year == y),]
  xw5w <- w5w[which(w5w$Year == y),]
  xw10w <- w10w[which(w10w$Year == y),]
  xwmarw <- wmarw[which(wmarw$Year == y),]

  xw1m <- w1m[which(w1m$Year == y),]
  xw2m <- w2m[which(w2m$Year == y),]
  xw4m <- w4m[which(w4m$Year == y),]
  xw8m <- w8m[which(w8m$Year == y),]
  xw15m <- w15m[which(w15m$Year == y),]
  xw5m <- w5m[which(w5m$Year == y),]
  xw10m <- w10m[which(w10m$Year == y),]
  xwmarm <- wmarm[which(wmarm$Year == y),]

  xu1w <- u1w[which(u1w$Year == y),]
  xu2w <- u2w[which(u2w$Year == y),]
  xu4w <- u4w[which(u4w$Year == y),]
  xu8w <- u8w[which(u8w$Year == y),]
  xu15w <- u15w[which(u15w$Year == y),]
  xu5w <- u5w[which(u5w$Year == y),]
  xu10w <- u10w[which(u10w$Year == y),]
  xumarw <- umarw[which(umarw$Year == y),]

  xu1m <- u1m[which(u1m$Year == y),]
  xu2m <- u2m[which(u2m$Year == y),]
  xu4m <- u4m[which(u4m$Year == y),]
  xu8m <- u8m[which(u8m$Year == y),]
  xu15m <- u15m[which(u15m$Year == y),]
  xu5m <- u5m[which(u5m$Year == y),]
  xu10m <- u10m[which(u10m$Year == y),]
  xumarm <- umarm[which(umarm$Year == y),]

  for (i in 1:1000) {

    pd.100 <- c(pd.100, 100 * (as.numeric(xw1w$Seconds[i]) - as.numeric(xw1m$Seconds[i])) / as.numeric(xw1m$Seconds[i]))
    pd.200 <- c(pd.200, 100 * (as.numeric(xw2w$Seconds[i]) - as.numeric(xw2m$Seconds[i])) / as.numeric(xw2m$Seconds[i]))
    pd.400 <- c(pd.400, 100 * (as.numeric(xw4w$Seconds[i]) - as.numeric(xw4m$Seconds[i])) / as.numeric(xw4m$Seconds[i]))
    pd.800 <- c(pd.800, 100 * (as.numeric(xw8w$Seconds[i]) - as.numeric(xw8m$Seconds[i])) / as.numeric(xw8m$Seconds[i]))
    pd.1500 <- c(pd.1500, 100 * (as.numeric(xw15w$Seconds[i]) - as.numeric(xw15m$Seconds[i])) / as.numeric(xw15m$Seconds[i]))
    pd.5000 <- c(pd.5000, 100 * (as.numeric(xw5w$Seconds[i]) - as.numeric(xw5m$Seconds[i])) / as.numeric(xw5m$Seconds[i]))
    pd.10000 <- c(pd.10000, 100 * (as.numeric(xw10w$Seconds[i]) - as.numeric(xw10m$Seconds[i])) / as.numeric(xw10m$Seconds[i]))
    pd.mar <- c(pd.mar, 100 * (as.numeric(xwmarw$Seconds[i]) - as.numeric(xwmarm$Seconds[i])) / as.numeric(xwmarm$Seconds[i]))

    upd.100 <- c(upd.100, 100 * (as.numeric(xu1w$Seconds[i]) - as.numeric(xu1m$Seconds[i])) / as.numeric(xu1m$Seconds[i]))
    upd.200 <- c(upd.200, 100 * (as.numeric(xu2w$Seconds[i]) - as.numeric(xu2m$Seconds[i])) / as.numeric(xu2m$Seconds[i]))
    upd.400 <- c(upd.400,100 *  (as.numeric(xu4w$Seconds[i]) - as.numeric(xu4m$Seconds[i])) / as.numeric(xu4m$Seconds[i]))
    upd.800 <- c(upd.800, 100 * (as.numeric(xu8w$Seconds[i]) - as.numeric(xu8m$Seconds[i])) / as.numeric(xu8m$Seconds[i]))
    upd.1500 <- c(upd.1500, 100 * (as.numeric(xu15w$Seconds[i]) - as.numeric(xu15m$Seconds[i])) / as.numeric(xu15m$Seconds[i]))
    upd.5000 <- c(upd.5000, 100 * (as.numeric(xu5w$Seconds[i]) - as.numeric(xu5m$Seconds[i])) / as.numeric(xu5m$Seconds[i]))
    upd.10000 <- c(upd.10000, 100 * (as.numeric(xu10w$Seconds[i]) - as.numeric(xu10m$Seconds[i])) / as.numeric(xu10m$Seconds[i]))
    upd.mar <- c(upd.mar, 100 * (as.numeric(xumarw$Seconds[i]) - as.numeric(xumarm$Seconds[i])) / as.numeric(xumarm$Seconds[i]))

  }

}

time.col <- c()
for (y in 2001:2022) {time.col <- c(time.col, rep(y-2000, 1000))}

rank.col <- c()
for (y in 2001:2022) {rank.col <- c(rank.col, seq(1,1000,1))}

w1pd <- as.data.frame(cbind(pd.100, rank.col, time.col))
w2pd <- as.data.frame(cbind(pd.200, rank.col, time.col))
w4pd <- as.data.frame(cbind(pd.400, rank.col, time.col))
w8pd <- as.data.frame(cbind(pd.800, rank.col, time.col))
w15pd <- as.data.frame(cbind(pd.1500, rank.col, time.col))
w5pd <- as.data.frame(cbind(pd.5000, rank.col, time.col))
w10pd <- as.data.frame(cbind(pd.10000, rank.col, time.col))
wmarpd <- as.data.frame(cbind(pd.mar, rank.col, time.col))

u1pd <- as.data.frame(cbind(upd.100, rank.col, time.col))
u2pd <- as.data.frame(cbind(upd.200, rank.col, time.col))
u4pd <- as.data.frame(cbind(upd.400, rank.col, time.col))
u8pd <- as.data.frame(cbind(upd.800, rank.col, time.col))
u15pd <- as.data.frame(cbind(upd.1500, rank.col, time.col))
u5pd <- as.data.frame(cbind(upd.5000, rank.col, time.col))
u10pd <- as.data.frame(cbind(upd.10000, rank.col, time.col))
umarpd <- as.data.frame(cbind(upd.mar, rank.col, time.col))

colnames(w1pd) <- c('PD', 'Rank', 'Time')
colnames(w2pd) <- c('PD', 'Rank', 'Time')
colnames(w4pd) <- c('PD', 'Rank', 'Time')
colnames(w8pd) <- c('PD', 'Rank', 'Time')
colnames(w15pd) <- c('PD', 'Rank', 'Time')
colnames(w5pd) <- c('PD', 'Rank', 'Time')
colnames(w10pd) <- c('PD', 'Rank', 'Time')
colnames(wmarpd) <- c('PD', 'Rank', 'Time')

colnames(u1pd) <- c('PD', 'Rank', 'Time')
colnames(u2pd) <- c('PD', 'Rank', 'Time')
colnames(u4pd) <- c('PD', 'Rank', 'Time')
colnames(u8pd) <- c('PD', 'Rank', 'Time')
colnames(u15pd) <- c('PD', 'Rank', 'Time')
colnames(u5pd) <- c('PD', 'Rank', 'Time')
colnames(u10pd) <- c('PD', 'Rank', 'Time')
colnames(umarpd) <- c('PD', 'Rank', 'Time')

u1pd <- u1pd[which(complete.cases(u1pd) == TRUE),]
u2pd <- u2pd[which(complete.cases(u2pd) == TRUE),]
u4pd <- u4pd[which(complete.cases(u4pd) == TRUE),]
u8pd <- u8pd[which(complete.cases(u8pd) == TRUE),]
u15pd <- u15pd[which(complete.cases(u15pd) == TRUE),]
u5pd <- u5pd[which(complete.cases(u5pd) == TRUE),]
u10pd <- u10pd[which(complete.cases(u10pd) == TRUE),]
umarpd <- umarpd[which(complete.cases(umarpd) == TRUE),]

# Replicating prior analysis

w800.1 <- lm(Seconds ~ Time, data = u8w[which(u8w$Rank3 == 25),])
w800.2 <- lm(Seconds ~ Time, data = u8w[which(u8w$Rank3 <= 25),])

w1500.1 <- lm(Seconds ~ Time, data = u15w[which(u15w$Rank3 == 25),])
w1500.2 <- lm(Seconds ~ Time, data = u15w[which(u15w$Rank3 <= 25),])

w5000.1 <- lm(Seconds ~ Time, data = u5w[which(u5w$Rank3 == 25),])
w5000.2 <- lm(Seconds ~ Time, data = u5w[which(u5w$Rank3 <= 25),])

w10000.1 <- lm(Seconds ~ Time, data = u10w[which(u10w$Rank3 == 25),])
w10000.2 <- lm(Seconds ~ Time, data = u10w[which(u10w$Rank3 <= 25),])

wmar.1 <- lm(Seconds ~ Time, data = umarw[which(umarw$Rank3 == 25),])
wmar.2 <- lm(Seconds ~ Time, data = umarw[which(umarw$Rank3 <= 25),])

m800.1 <- lm(Seconds ~ Time, data = u8m[which(u8m$Rank3 == 25),])
m800.2 <- lm(Seconds ~ Time, data = u8m[which(u8m$Rank3 <= 25),])

m1500.1 <- lm(Seconds ~ Time, data = u15m[which(u15m$Rank3 == 25),])
m1500.2 <- lm(Seconds ~ Time, data = u15m[which(u15m$Rank3 <= 25),])

m5000.1 <- lm(Seconds ~ Time, data = u5m[which(u5m$Rank3 == 25),])
m5000.2 <- lm(Seconds ~ Time, data = u5m[which(u5m$Rank3 <= 25),])

m10000.1 <- lm(Seconds ~ Time, data = u10m[which(u10m$Rank3 == 25),])
m10000.2 <- lm(Seconds ~ Time, data = u10m[which(u10m$Rank3 <= 25),])

mmar.1 <- lm(Seconds ~ Time, data = umarm[which(umarm$Rank3 == 25),])
mmar.2 <- lm(Seconds ~ Time, data = umarm[which(umarm$Rank3 <= 25),])

pd800.1 <- lm(PD ~ Time, data = u8pd[which(u8pd$Rank == 25),])
pd800.2 <- lm(PD ~ Time, data = u8pd[which(u8pd$Rank <= 25),])

pd1500.1 <- lm(PD ~ Time, data = u15pd[which(u15pd$Rank == 25),])
pd1500.2 <- lm(PD ~ Time, data = u15pd[which(u15pd$Rank <= 25),])

pd5000.1 <- lm(PD ~ Time, data = u5pd[which(u5pd$Rank == 25),])
pd5000.2 <- lm(PD ~ Time, data = u5pd[which(u5pd$Rank <= 25),])

pd10000.1 <- lm(PD ~ Time, data = u10pd[which(u10pd$Rank == 25),])
pd10000.2 <- lm(PD ~ Time, data = u10pd[which(u10pd$Rank <= 25),])

pdmar.1 <- lm(PD ~ Time, data = umarpd[which(umarpd$Rank == 25),])
pdmar.2 <- lm(PD ~ Time, data = umarpd[which(umarpd$Rank <= 25),])

# Expanding to the sprints

w100.1 <- lm(Seconds ~ Time, data = u1w[which(u1w$Rank3 == 25),])
w100.2 <- lm(Seconds ~ Time, data = u1w[which(u1w$Rank3 <= 25),])

w200.1 <- lm(Seconds ~ Time, data = u2w[which(u2w$Rank3 == 25),])
w200.2 <- lm(Seconds ~ Time, data = u2w[which(u2w$Rank3 <= 25),])

w400.1 <- lm(Seconds ~ Time, data = u4w[which(u4w$Rank3 == 25),])
w400.2 <- lm(Seconds ~ Time, data = u4w[which(u4w$Rank3 <= 25),])

m100.1 <- lm(Seconds ~ Time, data = u1m[which(u1m$Rank3 == 25),])
m100.2 <- lm(Seconds ~ Time, data = u1m[which(u1m$Rank3 <= 25),])

m200.1 <- lm(Seconds ~ Time, data = u2m[which(u2m$Rank3 == 25),])
m200.2 <- lm(Seconds ~ Time, data = u2m[which(u2m$Rank3 <= 25),])

m400.1 <- lm(Seconds ~ Time, data = u4m[which(u4m$Rank3 == 25),])
m400.2 <- lm(Seconds ~ Time, data = u4m[which(u4m$Rank3 <= 25),])

pd100.1 <- lm(PD ~ Time, data = u1pd[which(u1pd$Rank == 25),])
pd100.2 <- lm(PD ~ Time, data = u1pd[which(u1pd$Rank <= 25),])

pd200.1 <- lm(PD ~ Time, data = u2pd[which(u2pd$Rank == 25),])
pd200.2 <- lm(PD ~ Time, data = u2pd[which(u2pd$Rank <= 25),])

pd400.1 <- lm(PD ~ Time, data = u4pd[which(u4pd$Rank == 25),])
pd400.2 <- lm(PD ~ Time, data = u4pd[which(u4pd$Rank <= 25),])

# Viewing the results

stargazer(w800.1, w1500.1, w5000.1, w10000.1, wmar.1, type = 'text')
stargazer(m800.1, m1500.1, m5000.1, m10000.1, mmar.1, type = 'text')
stargazer(pd800.1, pd1500.1, pd5000.1, pd10000.1, pdmar.1, type = 'text')

stargazer(w800.2, w1500.2, w5000.2, w10000.2, wmar.2, type = 'text')
stargazer(m800.2, m1500.2, m5000.2, m10000.2, mmar.2, type = 'text')
stargazer(pd800.2, pd1500.2, pd5000.2, pd10000.2, pdmar.2, type = 'text')

stargazer(w100.1, w200.1, w400.1, type = 'text')
stargazer(m100.1, m200.1, m400.1, type = 'text')
stargazer(pd100.1, pd200.1, pd400.1, type = 'text')

stargazer(w100.2, w200.2, w400.2, type = 'text')
stargazer(m100.2, m200.2, m400.2, type = 'text')
stargazer(pd100.2, pd200.2, pd400.2, type = 'text')

# Expanding all of above regressions to all WA data

aw800.1 <- lm(Seconds ~ Time, data = w8w[which(w8w$Rank == 25),])
aw800.2 <- lm(Seconds ~ Time, data = w8w[which(w8w$Rank <= 25),])

aw1500.1 <- lm(Seconds ~ Time, data = w15w[which(w15w$Rank == 25),])
aw1500.2 <- lm(Seconds ~ Time, data = w15w[which(w15w$Rank <= 25),])

aw5000.1 <- lm(Seconds ~ Time, data = w5w[which(w5w$Rank == 25),])
aw5000.2 <- lm(Seconds ~ Time, data = w5w[which(w5w$Rank <= 25),])

aw10000.1 <- lm(Seconds ~ Time, data = w10w[which(w10w$Rank == 25),])
aw10000.2 <- lm(Seconds ~ Time, data = w10w[which(w10w$Rank <= 25),])

awmar.1 <- lm(Seconds ~ Time, data = wmarw[which(wmarw$Rank == 25),])
awmar.2 <- lm(Seconds ~ Time, data = wmarw[which(wmarw$Rank <= 25),])

am800.1 <- lm(Seconds ~ Time, data = w8m[which(w8m$Rank == 25),])
am800.2 <- lm(Seconds ~ Time, data = w8m[which(w8m$Rank <= 25),])

am1500.1 <- lm(Seconds ~ Time, data = w15m[which(w15m$Rank == 25),])
am1500.2 <- lm(Seconds ~ Time, data = w15m[which(w15m$Rank <= 25),])

am5000.1 <- lm(Seconds ~ Time, data = w5m[which(w5m$Rank == 25),])
am5000.2 <- lm(Seconds ~ Time, data = w5m[which(w5m$Rank <= 25),])

am10000.1 <- lm(Seconds ~ Time, data = w10m[which(w10m$Rank == 25),])
am10000.2 <- lm(Seconds ~ Time, data = w10m[which(w10m$Rank <= 25),])

ammar.1 <- lm(Seconds ~ Time, data = wmarm[which(wmarm$Rank == 25),])
ammar.2 <- lm(Seconds ~ Time, data = wmarm[which(wmarm$Rank <= 25),])

apd800.1 <- lm(PD ~ Time, data = w8pd[which(w8pd$Rank == 25),])
apd800.2 <- lm(PD ~ Time, data = w8pd[which(w8pd$Rank <= 25),])

apd1500.1 <- lm(PD ~ Time, data = w15pd[which(w15pd$Rank == 25),])
apd1500.2 <- lm(PD ~ Time, data = w15pd[which(w15pd$Rank <= 25),])

apd5000.1 <- lm(PD ~ Time, data = w5pd[which(w5pd$Rank == 25),])
apd5000.2 <- lm(PD ~ Time, data = w5pd[which(w5pd$Rank <= 25),])

apd10000.1 <- lm(PD ~ Time, data = w10pd[which(w10pd$Rank == 25),])
apd10000.2 <- lm(PD ~ Time, data = w10pd[which(w10pd$Rank <= 25),])

apdmar.1 <- lm(PD ~ Time, data = wmarpd[which(wmarpd$Rank == 25),])
apdmar.2 <- lm(PD ~ Time, data = wmarpd[which(wmarpd$Rank <= 25),])

# Expanding to the sprints

aw100.1 <- lm(Seconds ~ Time, data = w1w[which(w1w$Rank == 25),])
aw100.2 <- lm(Seconds ~ Time, data = w1w[which(w1w$Rank <= 25),])

aw200.1 <- lm(Seconds ~ Time, data = w2w[which(w2w$Rank == 25),])
aw200.2 <- lm(Seconds ~ Time, data = w2w[which(w2w$Rank <= 25),])

aw400.1 <- lm(Seconds ~ Time, data = w4w[which(w4w$Rank == 25),])
aw400.2 <- lm(Seconds ~ Time, data = w4w[which(w4w$Rank <= 25),])

am100.1 <- lm(Seconds ~ Time, data = w1m[which(w1m$Rank == 25),])
am100.2 <- lm(Seconds ~ Time, data = w1m[which(w1m$Rank <= 25),])

am200.1 <- lm(Seconds ~ Time, data = w2m[which(w2m$Rank == 25),])
am200.2 <- lm(Seconds ~ Time, data = w2m[which(w2m$Rank <= 25),])

am400.1 <- lm(Seconds ~ Time, data = w4m[which(w4m$Rank == 25),])
am400.2 <- lm(Seconds ~ Time, data = w4m[which(w4m$Rank <= 25),])

apd100.1 <- lm(PD ~ Time, data = w1pd[which(w1pd$Rank == 25),])
apd100.2 <- lm(PD ~ Time, data = w1pd[which(w1pd$Rank <= 25),])

apd200.1 <- lm(PD ~ Time, data = w2pd[which(w2pd$Rank == 25),])
apd200.2 <- lm(PD ~ Time, data = w2pd[which(w2pd$Rank <= 25),])

apd400.1 <- lm(PD ~ Time, data = w4pd[which(w4pd$Rank == 25),])
apd400.2<- lm(PD ~ Time, data = w4pd[which(w4pd$Rank <= 25),])

# Viewing the results

stargazer(aw800.1, aw1500.1, aw5000.1, aw10000.1, awmar.1, type = 'text')
stargazer(am800.1, am1500.1, am5000.1, am10000.1, ammar.1, type = 'text')
stargazer(apd800.1, apd1500.1, apd5000.1, apd10000.1, apdmar.1, type = 'text')

stargazer(aw800.2, aw1500.2, aw5000.2, aw10000.2, awmar.2, type = 'text')
stargazer(am800.2, am1500.2, am5000.2, am10000.2, ammar.2, type = 'text')
stargazer(apd800.2, apd1500.2, apd5000.2, apd10000.2, apdmar.2, type = 'text')

stargazer(aw100.1, aw200.1, aw400.1, type = 'text')
stargazer(am100.1, am200.1, am400.1, type = 'text')
stargazer(apd100.1, apd200.1, apd400.1, type = 'text')

stargazer(aw100.2, aw200.2, aw400.2, type = 'text')
stargazer(am100.2, am200.2, am400.2, type = 'text')
stargazer(apd100.2, apd200.2, apd400.2, type = 'text')

# Better analysis go brrr

women.wa$Seconds <- as.numeric(women.wa$Seconds)
women.us$Seconds <- as.numeric(women.us$Seconds)

men.wa$Seconds <- as.numeric(men.wa$Seconds)
men.us$Seconds <- as.numeric(men.us$Seconds)

all.wa <- rbind(women.wa, men.wa)
all.us <- rbind(women.us, men.us)

w1pd$Event <- rep('100-metres', dim(w1pd)[1])
w2pd$Event <- rep('200-metres', dim(w2pd)[1])
w4pd$Event <- rep('400-metres', dim(w4pd)[1])
w8pd$Event <- rep('800-metres', dim(w8pd)[1])
w15pd$Event <- rep('1500-metres', dim(w15pd)[1])
w5pd$Event <- rep('5000-metres', dim(w5pd)[1])
w10pd$Event <- rep('10000-metres', dim(w10pd)[1])
wmarpd$Event <- rep('marathon', dim(wmarpd)[1])

u1pd$Event <- rep('100-metres', dim(u1pd)[1])
u2pd$Event <- rep('200-metres', dim(u2pd)[1])
u4pd$Event <- rep('400-metres', dim(u4pd)[1])
u8pd$Event <- rep('800-metres', dim(u8pd)[1])
u15pd$Event <- rep('1500-metres', dim(u15pd)[1])
u5pd$Event <- rep('5000-metres', dim(u5pd)[1])
u10pd$Event <- rep('10000-metres', dim(u10pd)[1])
umarpd$Event <- rep('marathon', dim(umarpd)[1])

pd.wa <- rbind(w1pd, w2pd, w4pd, w8pd, w15pd, w5pd, w10pd, wmarpd)
pd.us <- rbind(u1pd, u2pd, u4pd, u8pd, u15pd, u5pd, u10pd, umarpd)

w.w.all <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = women.wa)
w.w.dis <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = women.wa[which(women.wa$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
w.w.spr <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = women.wa[which(women.wa$Event %in% c('100-metres', '200-metres', '400-metres')),])

m.w.all <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = men.wa)
m.w.dis <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = men.wa[which(men.wa$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
m.w.spr <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = men.wa[which(men.wa$Event %in% c('100-metres', '200-metres', '400-metres')),])

a.w.all <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Gender)*factor(Event), data = all.wa)
a.w.dis <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Gender)*factor(Event), data = all.wa[which(all.wa$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
a.w.spr <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Gender)*factor(Event), data = all.wa[which(all.wa$Event %in% c('100-metres', '200-metres', '400-metres')),])

pd.w.all <- lm(PD ~ -1 + Rank + Time*factor(Event), data = pd.wa)
pd.w.dis <- lm(PD ~ -1 + Rank + Time*factor(Event), data = pd.wa[which(pd.wa$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
pd.w.spr <- lm(PD ~ -1 + Rank + Time*factor(Event), data = pd.wa[which(pd.wa$Event %in% c('100-metres', '200-metres', '400-metres')),])

pd.w.all2 <- lm(PD ~ -1 + Rank*Time + Time*factor(Event), data = pd.wa)
pd.w.dis2 <- lm(PD ~ -1 + Rank*Time + Time*factor(Event), data = pd.wa[which(pd.wa$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
pd.w.spr2 <- lm(PD ~ -1 + Rank*Time + Time*factor(Event), data = pd.wa[which(pd.wa$Event %in% c('100-metres', '200-metres', '400-metres')),])

w.u.all <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = women.us)
w.u.dis <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = women.us[which(women.us$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
w.u.spr <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = women.us[which(women.us$Event %in% c('100-metres', '200-metres', '400-metres')),])

m.u.all <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = men.us)
m.u.dis <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = men.us[which(men.us$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
m.u.spr <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Event), data = men.us[which(men.us$Event %in% c('100-metres', '200-metres', '400-metres')),])

a.u.all <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Gender)*factor(Event), data = all.us)
a.u.dis <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Gender)*factor(Event), data = all.us[which(all.us$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
a.u.spr <- lm(log(Seconds) ~ -1 + Rank + Time*factor(Gender)*factor(Event), data = all.us[which(all.us$Event %in% c('100-metres', '200-metres', '400-metres')),])

pd.u.all <- lm(PD ~ -1 + Rank + Time*factor(Event), data = pd.us)
pd.u.dis <- lm(PD ~ -1 + Rank + Time*factor(Event), data = pd.us[which(pd.us$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
pd.u.spr <- lm(PD ~ -1 + Rank + Time*factor(Event), data = pd.us[which(pd.us$Event %in% c('100-metres', '200-metres', '400-metres')),])

pd.u.all2 <- lm(PD ~ -1 + Rank*Time + Time*factor(Event), data = pd.us)
pd.u.dis2 <- lm(PD ~ -1 + Rank*Time + Time*factor(Event), data = pd.us[which(pd.us$Event %in% c('800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon')),])
pd.u.spr2 <- lm(PD ~ -1 + Rank*Time + Time*factor(Event), data = pd.us[which(pd.us$Event %in% c('100-metres', '200-metres', '400-metres')),])

# Viewing the results

stargazer(w.w.all, w.w.dis, w.w.spr, type = 'text')
stargazer(m.w.all, m.w.dis, m.w.spr, type = 'text')
stargazer(a.w.all, a.w.dis, a.w.spr, type = 'text')
stargazer(pd.w.all, pd.w.dis, pd.w.spr, type = 'text')
stargazer(pd.w.all2, pd.w.dis2, pd.w.spr2, type = 'text')

stargazer(w.u.all, w.u.dis, w.u.spr, type = 'text')
stargazer(m.u.all, m.u.dis, m.u.spr, type = 'text')
stargazer(a.u.all, a.u.dis, a.u.spr, type = 'text')
stargazer(pd.u.all, pd.u.dis, pd.u.spr, type = 'text')
stargazer(pd.u.all2, pd.u.dis2, pd.u.spr2, type = 'text')

stargazer(w.w.all, w.u.all, w.w.dis, w.u.dis, w.w.spr, w.u.spr, type = 'text', omit.stat = c('ser', 'f'))
stargazer(m.w.all, m.u.all, m.w.dis, m.u.dis, m.w.spr, m.u.spr, type = 'text', omit.stat = c('ser', 'f'))
stargazer(a.w.all, a.u.all, a.w.dis, a.u.dis, a.w.spr, a.u.spr, type = 'text', omit.stat = c('ser', 'f'))
stargazer(pd.w.all, pd.u.all, pd.w.dis, pd.u.dis, pd.w.spr, pd.u.spr, type = 'text', omit.stat = c('ser', 'f'))
stargazer(pd.w.all2, pd.u.all2, pd.w.dis2, pd.u.dis2, pd.w.spr2, pd.u.spr2, type = 'text', omit.stat = c('ser', 'f'))

