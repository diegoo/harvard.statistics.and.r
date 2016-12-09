library(rafalib)
library(dplyr)

dat <- read.csv("femaleMiceWeights.csv")

## Suppose we are interested in the proportion of times we see a 6 when rolling n=100 die. This is a random variable which we can simulate with x=sample(1:6, n, replace=TRUE) and the proportion we are interested in can be expressed as an average: mean(x==6). Because the die rolls are independent, the CLT applies.

## We want to roll n dice 10,000 times and keep these proportions. This random variable (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n. So according to CLT z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1. Set the seed to 1, then use replicate to perform the simulation, and report what proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05).

## x = sample(1:6, n, replace=TRUE)
## z = (mean(x==6) - p) / sqrt(p*(1-p)/n)

set.seed(1)
p = 1/6
n <- 100
times <- 10000
simulation <- replicate(times, (mean(sample(1:6, n, replace=TRUE) == 6) - p) / sqrt(p * (1-p)/n))
length(simulation[abs(simulation) > 2]) / 10000

## lo mismo

set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1) ## confirm it's well approximated with normal distribution
mean(abs(zs) > 2)
