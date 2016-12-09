x <- unlist(read.csv("femaleControlsPopulation.csv"))

## Using the same process as before (in Null Distribution Exercises),
## set the seed at 1, then using a for-loop take a random sample of 5
## mice 1,000 times. Save these averages. After that, set the seed at
## 1, then using a for-loop take a random sample of 50 mice 1,000
## times. Save these averages.

set.seed(1)
n <- 1000
averages.5.1k <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages.5.1k[i] <- mean(X)
}
hist(averages.5.1k)

set.seed(1)
n <- 1000
averages.50.1k <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages.50.1k[i] <- mean(X)
}
hist(averages.50.1k)

## For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 23 and 25?
mean(averages.50.1k <= 25) - mean(averages.50.1k <= 23)
## lo mismo
mean(averages.50.1k < 25 & averages.50.1k > 23)

## Now ask the same question of a normal distribution with average 23.9 and standard deviation 0.43.
normal.distribution <- rnorm(1000, mean = 23.9, sd = 0.43)
mean(normal.distribution < 25 & normal.distribution > 23)
## lo mismo
pnorm((25-23.9) / 0.43) - pnorm((23-23.9) / 0.43) 
