library(rafalib)
library(dplyr)

dat <- read.csv("femaleMiceWeights.csv")

## Now we introduce the concept of a null hypothesis. We don't know μX nor μY. We want to quantify what the data say about the possibility that the diet has no effect: μX=μY. If we use CLT, then we approximate the distribution of X¯ as normal with mean μX and standard deviation σX/M and the distribution of Y¯ as normal with mean μY and standard deviation σY/N, with M and N the sample sizes for X and Y respectively, in this case 12. This implies that the difference Y¯ - X¯ has mean 0. We described that the standard deviation of this statistic (the standard error) is SE(X¯ - Y¯) = σY2/12 + σX2/12 and that we estimate the population standard deviations σX and σY with the sample estimates. What is the estimate of SE(X¯ - Y¯) = σY2/12 + σX2/12 ?

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

sqrt(sd(X)**2/12 + sd(Y)**2/12)
## lo mismo
sqrt(var(X)/12 + var(Y)/12)

## So now we can compute Y¯−X¯ as well as an estimate of this standard error and construct a t-statistic. What number is this t-statistic?

(mean(Y) - mean(X)) / sqrt(var(Y)/12 + var(X)/12)

## lo mismo
t.test(Y, X)$stat

## the t-distribution is centered at 0 and has one parameter: the degrees of freedom, that control the size of the tails. You will notice that if X follows a t-distribution the probability that X is smaller than an extreme value such as 3 SDs away from the mean grows with the degrees of freedom. For example, notice the difference between:

## 1 - pt(3,df=3)
## 1 - pt(3,df=15)
## 1 - pt(3,df=30)
## 1 - pnorm(3)

## As we explained, under certain assumptions, the t-statistic follows a t-distribution. Determining the degrees of freedom can sometimes be cumbersome, but the t.test function calculates it for you. One important fact to keep in mind is that the degrees of freedom are directly related to the sample size. There are various resources for learning more about degrees of freedom on the internet as well as statistics books.

## #10 If we apply the CLT, what is the distribution of this t-statistic?
## respuesta correcta: Normal with mean 0 and standard deviation 1.

## #11
(1 - pnorm(t.test(Y, X)$stat)) * 2
## lo mismo
Z <- (mean(Y) - mean(X)) / sqrt(var(X)/12 + var(Y)/12)
(1 - pnorm(Z)) * 2

## #12
t.test(X,Y)$p.value

## #13

## With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, one that is larger. They can't both be right. What best describes the difference?
## These are two different assumptions. The t-distribution accounts for the variability introduced by the estimation of the standard error and thus, under the null, large values are more probable under the null distribution.
