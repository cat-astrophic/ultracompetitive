# This script runs simulations for the gender and competitiveness project

# Loading libraries

library(ggplot2)
library(dplyr)

# A simulation for the paper using percentiles

binner <- function (veccy) {
  
  outvec <- c()
  
  for (i in 1:100) {
    
    outvec <- c(outvec, mean(veccy[(1000*(i-1)+1):(i*1000)]))
    
  }
  
  return(outvec)
  
}

set.seed(42069)

f.data <- c()
m.data <- c()
sim.data <- c()

for (it in 1:100) {
  
  print(it)
  
  DM <- rnorm(100000, mean = 100, sd = 10)
  DF <- rnorm(100000, mean = 105, sd = 10)
  
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
  scale_x_continuous(breaks = c(1, seq(10, 100, 10)), labels = c(1, seq(10, 100, 10)))

ggplot(data = plot.df, aes(x = X, y = Mean)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Percentile') +
  ylab('Percent Difference') +
  xlab('Percentile') +
  geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'orange') +
  geom_hline(yintercept = 5, color = 'black', linetype = 3) +
  geom_line(aes(y = Mean), size = 1, alpha = 1, color = 'red4') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1, seq(10, 100, 10)), labels = c(1, seq(10, 100, 10)))

sim.df <- as.data.frame(c(f.data, m.data))
sim.df$Gender <- c(rep('F', length(f.data)), rep('M', length(m.data)))
names(sim.df) <- c('Values', 'Gender')

ggplot(data = sim.df, aes(Values, fill = Gender)) +
  theme_bw() +
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

for (it in 1:100) {
  
  print(it)
  
  DM2 <- rnorm(100000, mean = 100, sd = 10)
  DF2 <- rnorm(100000, mean = 105, sd = 10)
  
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
  theme_bw() +
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
                     values = c('1' = 'red4', '2' = 'orange', '5' = 'green', '10' = 'blue', '20' = 'purple')) +
  scale_x_continuous(breaks = c(1, seq(50, 500, 50)), labels = c(1, seq(50, 500, 50)))

# A simulation for the paper using place with a gendered sampling component

sim.data4 <- c()
sim.data4.2 <- c()
sim.data4.3 <- c()
sim.data4.4 <- c()
sim.data4.5 <- c()
sim.data4.10 <- c()
sim.data4.20 <- c()
sim.data4.100 <- c()

for (it in 1:100) {
  
  print(it)
  
  DM4 <- rnorm(100000, mean = 100, sd = 10)
  DF4 <- rnorm(100000, mean = 105, sd = 10)
  
  DM4 <- sort(DM4, decreasing = FALSE)
  DF4 <- sort(DF4, decreasing = FALSE)
  
  pct.diff <- 100*(DF4[1:1000] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.2 <- 100*(DF4[seq(2,2000,2)] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.3 <- 100*(DF4[seq(3,3000,3)] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.4 <- 100*(DF4[seq(4,4000,4)] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.5 <- 100*(DF4[seq(5,5000,5)] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.10 <- 100*(DF4[seq(10,10000,10)] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.20 <- 100*(DF4[seq(20,20000,20)] - DM4[1:1000]) / DM4[1:1000]
  pct.diff.100 <- 100*(DF4[seq(100,100000,100)] - DM4[1:1000]) / DM4[1:1000]
  
  sim.data4 <- cbind(sim.data4, pct.diff)
  sim.data4.2 <- cbind(sim.data4.2, pct.diff.2)
  sim.data4.3 <- cbind(sim.data4.3, pct.diff.3)
  sim.data4.4 <- cbind(sim.data4.4, pct.diff.4)
  sim.data4.5 <- cbind(sim.data4.5, pct.diff.5)
  sim.data4.10 <- cbind(sim.data4.10, pct.diff.10)
  sim.data4.20 <- cbind(sim.data4.20, pct.diff.20)
  sim.data4.100 <- cbind(sim.data4.100, pct.diff.100)
  
}

plot.df4 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4), apply(sim.data4, 1, sd)))
plot.df4.2 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.2), apply(sim.data4.2, 1, sd)))
plot.df4.3 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.3), apply(sim.data4.3, 1, sd)))
plot.df4.4 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.4), apply(sim.data4.4, 1, sd)))
plot.df4.5 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.5), apply(sim.data4.5, 1, sd)))
plot.df4.10 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.10), apply(sim.data4.10, 1, sd)))
plot.df4.20 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.20), apply(sim.data4.20, 1, sd)))
plot.df4.100 <- as.data.frame(cbind(1:1000, rowMeans(sim.data4.100), apply(sim.data4.100, 1, sd)))

names(plot.df4) <- c('X', 'Mean', 'SD')
names(plot.df4.2) <- c('X', 'Mean', 'SD')
names(plot.df4.3) <- c('X', 'Mean', 'SD')
names(plot.df4.4) <- c('X', 'Mean', 'SD')
names(plot.df4.5) <- c('X', 'Mean', 'SD')
names(plot.df4.10) <- c('X', 'Mean', 'SD')
names(plot.df4.20) <- c('X', 'Mean', 'SD')
names(plot.df4.100) <- c('X', 'Mean', 'SD')

combo.df <- as.data.frame(cbind(plot.df4$X, plot.df4$Mean, plot.df4.2$Mean, plot.df4.3$Mean, plot.df4.4$Mean, plot.df4.5$Mean, plot.df4.10$Mean, plot.df4.20$Mean, plot.df4.100$Mean, plot.df4$SD, plot.df4.2$SD, plot.df4.3$SD, plot.df4.4$SD, plot.df4.5$SD, plot.df4.10$SD, plot.df4.20$SD, plot.df4.100$SD))
colnames(combo.df) <- c('X', 'R1', 'R2', 'R3', 'R4', 'R5', 'R10', 'R20', 'R100', 'SD1', 'SD2', 'SD3', 'SD4', 'SD5', 'SD10', 'SD20', 'SD100')

ggplot(data = combo.df, aes(x = X, y = R1)) +
  theme_bw() +
  ggtitle('Percent Difference between Women and Men by Place') +
  ylab('Percent Difference') +
  xlab('Place') +
  geom_hline(yintercept = 5, color = 'black', linetype = 1) +
  geom_line(aes(y = R1, col = '1'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R2, col = '2'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R3, col = '3'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R4, col = '4'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R5, col = '5'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R10, col = '10'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R20, col = '20'), size = 1, alpha = 0.1) +
  geom_line(aes(y = R100, col = '100'), size = 1, alpha = 0.1) +
  geom_ribbon(aes(ymin = R1 - 2*SD1, ymax = R1 + 2*SD1), size = 1, alpha = .333, fill = 'red4') +
  geom_ribbon(aes(ymin = R2 - 2*SD2, ymax = R2 + 2*SD2), size = 1, alpha = .333, fill = 'orange') +
  geom_ribbon(aes(ymin = R3 - 2*SD3, ymax = R3 + 2*SD3), size = 1, alpha = .333, fill = 'yellow') +
  geom_ribbon(aes(ymin = R4 - 2*SD4, ymax = R4 + 2*SD4), size = 1, alpha = .333, fill = 'green') +
  geom_ribbon(aes(ymin = R5 - 2*SD5, ymax = R5 + 2*SD5), size = 1, alpha = .333, fill = 'blue') +
  geom_ribbon(aes(ymin = R10 - 2*SD10, ymax = R10 + 2*SD10), size = 1, alpha = .333, fill = 'purple') +
  geom_ribbon(aes(ymin = R20 - 2*SD20, ymax = R20 + 2*SD20), size = 1, alpha = .333, fill = 'violet') +
  geom_ribbon(aes(ymin = R100 - 2*SD100, ymax = R100 + 2*SD100), size = 1, alpha = .333, fill = 'brown4') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = 'Sample Ratio', breaks = c('1', '2', '3', '4', '5', '10', '20', '100'),
                     values = c('1' = 'red4', '2' = 'orange', '3' = 'yellow', '4' = 'green', '5' = 'blue', '10' = 'purple', '20' = 'violet', '100' = 'brown4')) +
  scale_x_continuous(breaks = c(1, seq(100, 1000, 100)), labels = c(1, seq(100, 1000, 100)))

