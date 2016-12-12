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


## For the last simulation you can make a qqplot to confirm the normal approximation. Now, the CLT is an asympototic result, meaning it is closer and closer to being a perfect approximation as the sample size increases. In practice, however, we need to decide if it is appropriate for actual sample sizes. Is 10 enough? 15? 30?
## In the example used in exercise 1, the original data is binary (either 6 or not). In this case, the success probability also affects the appropriateness of the CLT. With very low probabilities, we need larger sample sizes for the CLT to "kick in".
## Run the simulation from exercise 1, but for different values of p and n. For which of the following is the normal approximation best?

run_simulation <- function(p, n, sides) {
    set.seed(1)
    zs <- replicate(10000,{
        x <- sample(1:sides,n,replace=TRUE)
        (mean(x==sides) - p) / sqrt(p*(1-p)/n)
    }) 
    qqnorm(zs, main=paste("p=", p, "n=", n, "sides=", sides, sep=" "))
    abline(0,1)
}

par(mfrow=c(2,2))
run_simulation(0.5, 5, 2)
run_simulation(0.5, 30, 2)
run_simulation(0.01, 30, 100)
run_simulation(0.01, 100, 100)

## lo mismo

ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
  	x <- sample(1:sides,n,replace=TRUE)
  	(mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

## As we have already seen, the CLT also applies to averages of quantitative data. A major difference with binary data, for which we know the variance is p(1−p), is that with quantitative data we need to estimate the population standard deviation.

## In several previous exercises we have illustrated statistical concepts with the unrealistic situation of having access to the entire population. In practice, we do *not* have access to entire populations. Instead, we obtain one random sample and need to reach conclusions analyzing that data. dat is an example of a typical simple dataset representing just one sample. We have 12 measurements for each of two populations:

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

## We think of X as a random sample from the population of all mice in the control diet and Y as a random sample from the population of all mice in the high fat diet.
## What is the sample average?

sample.mean <- mean(X)

## CLT: when the sample size is large, the average of a random sample follows a normal distribution centered at the population average
## CLT: sample's standard deviation <- population's standard deviation / square root of the sample size

sample.size <- length(X)
sample.sd <- sqrt(1 / (sample.size - 1) * sum((X - sample.mean) ** 2))
## lo mismo:
sample.sd == sd(X)
## para estimar la desviacion estandar de la población, usamos la desviacion estandar de la muestra
population.sd <- sample.sd / sqrt(sample.size)

up <-(sample.mean + 2) / (sample.sd / sqrt(sample.size))
## [1] 29.58439
down <- (sample.mean-2) / (sample.sd/sqrt(sample.size))
## [1] 25.00003
range <- (up - down) / 2
## [1] 2.29218

## probabilidad de que la diferencia esté por abajo de -2 o por arriba de 2
pnorm(-2.29218) + (1 - pnorm(2.29218))
## [1] 0.02189526

## respuesta oficial:
2 * (1 - pnorm(2 / sd(X) * sqrt(12)))
