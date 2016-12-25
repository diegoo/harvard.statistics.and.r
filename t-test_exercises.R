library(rafalib)
library(dplyr)

babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
mean(bwt.nonsmoke) - mean(bwt.smoke)
##[1] 8.937666
popsd(bwt.nonsmoke)
##[1] 17.38696
popsd(bwt.smoke)
##[1] 18.08024

## The population difference of mean birth weights is about 8.9
## ounces. The standard deviations of the nonsmoking and smoking
## groups are about 17.4 and 18.1 ounces, respectively.

## We will treat the babies dataset as the full population and draw
## samples from it to simulate individual experiments. We will then
## ask whether somebody who only received the random samples would be
## able to draw correct conclusions about the population.

## Set the seed at 1 and obtain a samples from the non-smoking mothers
## (dat.ns) of size N=25. Then, without resetting the seed, take a
## sample of the same size from and smoking mothers (dat.s). Compute
## the t-statistic (call it tval).

N=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)
tval <- t.test(dat.ns, dat.s)
tval
##         Welch Two Sample t-test

## data:  dat.ns and dat.s
## t = 2.1209, df = 47.693, p-value = 0.03916
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   0.5141953 19.3258047
## sample estimates:
## mean of x mean of y 
##    124.68    114.76 

## lo mismo, a mano:
N = 25
X.ns <- mean(dat.ns)
X.s <- mean(dat.s)
sd.ns <- sd(dat.ns)
sd.s <- sd(dat.s)

sd.diff <- sqrt((sd.ns^2) / N + (sd.s^2) / N)
tval <- (X.ns - X.s) / sd.diff
tval
## [1] 2.120904

## Recall that we summarize our data using a t-statistics because we know that in situations where the null hypothesis is true (what we mean when we say "under the null") and the sample size is relatively large, this t-value will have an approximate standard normal distribution. Because we know the distribution of the t-value under the null, we can quantitatively determine how unusual the observed t-value would be if the null hypothesis were true.

## The standard procedure is to examine the probability a t-statistic that actually does follow the null hypothesis would have larger absolute value than the absolute value of the t-value we just observed -- this is called a two-sided test.

## We have computed these by taking one minus the area under the standard normal curve between -abs(tval) and abs(tval). In R, we can do this by using the pnorm function, which computes the area under a normal curve from negative infinity up to the value given as its first argument:

## What is the probability that the null hypothesis is true- the probability that data lies outside the normal distribution where t-test value is 2.1209

pval <- 1-(pnorm(abs(tval)) - pnorm(-abs(tval)))
pval
# 0.03392985

## Because of the symmetry of the standard normal distribution, there is a simpler way to calculate the probability that a t-value under the null could have a larger absolute value than tval:
2*pnorm(-abs(tval))

## It might help you (it has helped me) to draw the normal distribution on a piece of paper. Remember that the total area under the curve adds up to 1 and the left and right side are symmetric. It shows that calculating the area from the left untill -abs(tval) times 2 (because you have a left and right tail): 2*pnorm(-abs(tval))
## gives the same number as substracting (the left area untill abs(tval) minus the left area untill (-abs(tval))) from the total area under the curve (which is 1): 1- (pnorm(tval)-pnorm(-1*tval))

## By reporting only p-values, many scientific publications provide an incomplete story of their findings. As we have mentioned, with very large sample sizes, scientifically insignificant differences between two groups can lead to small p-values. Confidence intervals are more informative as they include the estimate itself. Our estimate of the difference between babies of smoker and non-smokers: mean(dat.s) - mean( dat.ns). If we use the CLT, what quantity would we add and subtract to this estimate to obtain a 99% confidence interval?

set.seed(1)
N <- 25
qnorm(0.995)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N)
