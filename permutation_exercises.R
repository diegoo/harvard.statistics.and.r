library(dplyr)

babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

## We will generate the following random variable based on a sample
## size of 10 and observe the following difference:

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obsdiff <- mean(smokers) - mean(nonsmokers)

## The question is whether this observed difference is statistically
## significant. We do not want to rely on the assumptions needed for
## the normal or t-distribution approximations to hold, so instead we
## will use permutations. We will reshuffle the data and recompute the
## mean. We can create one permuted sample with the following code:

dat <- c(smokers,nonsmokers)
shuffle <- sample(dat)
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar) - mean(nonsmokersstar)

## The last value is one observation from the null distribution we
## will construct. Set the seed at 1, and then repeat the permutation
## 1,000 times to create a null distribution. What is the permutation
## derived p-value for our observation?

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample(dat)
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar) - mean(nonsmokersstar)
})
## How many of the null means are bigger than the observed value? That
## proportion would be the p-value for the null. We add a 1 to the
## numerator and denominator to account for misestimation of the
## p-value (for more details see Phipson and Smyth, Permutation
## P-values should never be zero).
## the proportion of permutations with larger difference
p.value.for.the.null <- (sum(abs(null) >= abs(obsdiff)) +1) / (length(null)+1) 
## 0.05294705

## we add the 1s to avoid p-values=0 but we also accept:
## (sum(abs(null) >= abs(obs))) / (length(null))


## Repeat the above exercise, but instead of the differences in mean,
## consider the differences in median obs <- median(smokers) -
## median(nonsmokers). What is the permutation based p-value?

set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
median.obsdiff <- median(smokers) - median(nonsmokers)

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample(dat)
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  return(median(smokersstar) - median(nonsmokersstar))
})
(sum(abs(null) >= abs(median.obsdiff)) +1) / (length(null)+1) 
