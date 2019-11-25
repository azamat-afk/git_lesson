#Data distribution
library(ggplot2)
#take a SRS
loans_income <- read.csv('~/statistics-for-data-scientists/data/loans_income.csv')[,1]
samp_data <- data.frame(
  income=sample(loans_income, 1000),
                        type='data_dist')

#take a sample of means of 5 values
samp_mean_05 <- data.frame(
  income = tapply(sample(loans_income, 1000*5), 
                   rep(1:1000, rep(5, 1000)), FUN=mean),
type='mean_of_5')

#take a sample of means of 20 values
samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20),
                  rep(1:1000, rep(20, 1000)), FUN=mean),
  type='mean_of_20')
  
#bind the data.frames and convert type to a factor
income <- rbind(samp_data, samp_mean_05, samp_mean_20)

income$type = factor(income$type,
                     levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))

#plot the histograms
ggplot(income, aes(x=income))+
  geom_histogram(bins=40)+
  facet_grid(type ~ .)

library(boot)
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R=1000, statistic=stat_fun)
boot_obj

norm_samp <- rnorm(100)
qqnorm(norm_samp)
abline(a=0, b=1, col='grey')

nflx <- sp500_px[, 'NFLX']
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx)
abline(a=0, b=1, col='grey')

dbinom(x=2, size=5, p=0.1)

pbinom(2, 5, 0.1)

rpois(100, lambda=2)

rexp(n=100, rate=0.2)

rweibull(100, 1.5, 5000)