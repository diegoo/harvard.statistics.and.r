library(rafalib)
library(dplyr)

## CLT: when the sample size is large, the average μY of a random
## sample follows a normal distribution centered at the population
## average μy and with standard deviation equal to the population
## standard deviation σY, divided by the square root of the sample
## size N.

## standard deviation of the distribution of a random variable: random
## variable's standard error.

## --------------------------------------------------------------------------------

## If a list of numbers has a distribution that is well approximated
## by the normal distribution, what proportion of these numbers are
## within one standard deviation away from the list's average?

pnorm(1) - pnorm(-1)
## 0.6826895 

dat <- na.omit(read.csv("mice_pheno.csv"))

## Define y to be the weights of males on the control diet. What
## proportion of the mice are within one standard deviation away from
## the average weight (remember to use popsd for the population sd)?

y <- filter(dat, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
popsd(y)
## 4.420501
mean(y)
## 30.96381

nrow(subset(dat, Sex == 'M' & Diet == 'chow' & Bodyweight >= (30.96381-4.420501) & Bodyweight <= (30.96381+4.420501)))
## 155
155 / length(controlMales)
## 0.6950673

## lo mismo:

y <- filter(dat, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
z <- (y - mean(y)) / popsd(y)
mean(abs(z) <= 1)

## las proporciones del grupo de control dentro de 1, 2 y 3
## desviaciones estandar están cerca de las proporciones de una
## distribución normal dentro de 1, 2 y 3 desviaciones estandar

qqnorm(z)
abline(0,1)


## We will now take a sample of size 25 from the population of males
## on the chow diet. The average of this sample is our random
## variable. We will use the replicate to observe 10,000 realizations
## of this random variable. Set the seed at 1, generate these 10,000
## averages. Make a histogram and qq-plot of these 10,000 numbers
## against the normal distribution. We can see that, as predicted by
## the CLT, the distribution of the random variable is very well
## approximated by the normal distribution.

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean(sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)


