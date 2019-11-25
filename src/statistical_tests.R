#Statistical experiments

session_times <- read.csv("~/statistics-for-data-scientists/data/web_page_data.csv")

library(ggplot2)
ggplot(session_times, aes(x=Page, y=Time))+
  geom_boxplot()

mean_a <- mean(session_times[session_times['Page']=='Page A', 'Time'])
mean_a
mean_b <- mean(session_times[session_times['Page']=='Page B', 'Time'])
mean_b-mean_a

#Permutation test
perm_fun <- function(x, n1, n2)
{
  n <- n1+n2
  idx_b <- sample(1:n, n1)
  idx_a <- sample(1:n, n2)
  mean_diff <- mean(x[idx_b])-mean(x[idx_a])
  return(mean_diff)
}

perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = perm_fun(session_times[,'Time'], 21, 15)
hist(perm_diffs, xlab='Session time differences (in seconds)')
abline(v=mean_b-mean_a)

obs_pct_diff <- 100*(200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion, 23739, 22588)
hist(perm_diffs, xlab=' Conversion rate (percent)', main='')
abline(v=obs_pct_diff)

mean(perm_diffs > obs_pct_diff)

#test of two proportions
prop.test(x=c(200,182), n=c(23739, 22588), alternative="greater")

#t test
t.test(Time ~ Page, data=session_times, altrnative='less')

library(lmPerm)
four_sessions <- read.csv("~/statistics-for-data-scientists/data/four_sessions.csv")
summary(aovp(Time ~ Page, data=four_sessions))

#ANOVA 
summary(aov(Time~ Page, data=four_sessions))

click_rate <- read.csv("~/statistics-for-data-scientists/data/click_rates.csv")
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)
dimnames(clicks) <- list(unique(click_rate$Headline), unique(click_rate$Click))
chisq.test(clicks, simulate.p.value=TRUE)

chisq.test(clicks, simulate.p.value=FALSE)

fisher.test(clicks)

library(pwr)
pwr.2p.test(h=0.1, sig.level=0.05, power=0.85, alternative='greater')
