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

