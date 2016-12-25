library(rafalib)
library(dplyr)

babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

## The population difference of mean birth weights is about 8.9 ounces. The standard deviations of the nonsmoking and smoking groups are about 17.4 and 18.1 ounces, respectively.

## As we did with the mouse weight data, this assessment interactively reviews inference concepts using simulations in R. We will treat the babies dataset as the full population and draw samples from it to simulate individual experiments. We will then ask whether somebody who only received the random samples would be able to draw correct conclusions about the population.

## We are interested in testing whether the birth weights of babies born to non-smoking mothers are significantly different from the birth weights of babies born to smoking mothers.


## Set the seed at 1 and obtain two samples, each of size N = 25, from non-smoking mothers (dat.ns) and smoking mothers (dat.s). If instead of CLT, we use the t-distribution approximation, what do we add and subtract to obtain a 99% confidence interval (use 2*N-2 degrees of freedom)?

set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)
qt(1-0.01/2, df=2*N-2) * sqrt(sd(dat.ns)^2/N + sd(dat.s)^2/N)
## 12.54534

## No matter which way you compute it, the p-value pval is the probability that the null hypothesis could have generated a t-statistic more extreme than than what we observed: tval. If the p-value is very small, this means that observing a value more extreme than tval would be very rare if the null hypothesis were true, and would give strong evidence that we should **reject** the null hypothesis. We determine how small the p-value needs to be to reject the null by deciding how often we would be willing to mistakenly reject the null hypothesis.

## The standard decision rule is the following: choose some small value α (in most disciplines the conventional choice is α=0.05) and reject the null hypothesis if the p-value is less than α. We call α the _significance level_ of the test.

## It turns out that if we follow this decision rule, the probability that we will reject the null hypothesis by mistake is equal to α. (This fact is not immediately obvious and requires some probability theory to show.) We call the _event_ of rejecting the null hypothesis, when it is in fact true, a _Type I error_, we call the _probability_ of making a Type I error, the _Type I error rate_, and we say that rejecting the null hypothesis when the p-value is less than α, _controls_ the Type I error rate so that it is equal to α. We will see a number of decision rules that we use in order to control the probabilities of other types of errors. Often, we will guarantee that the probability of an error is less than some level, but, in this case, we can guarantee that the probability of a Type I error is _exactly equal_ to α.


## error tipo 1: creer que la hipótesis nula es falsa (i.e. rechazarla) cuando en realidad es cierta (i.e. falso positivo)
## error tipo 2: creer que la hipótesis nula es cierta (i.e. no rechazarla) cuando en realidad es falsa (i.e. falso negativo)
## power: sensitivity de la prueba = 1 - error tipo 2 (i.e. poder de la prueba es la probabilidad de rechazar la hipótesis nula cuando la alternativa es cierta). cuando aumenta el tamaño de la muestra, el poder aumenta => sirve para saber qué tamaño es necesario para tener un control sobre el error de tipo 2, que es el que realmente cuenta (error de tipo 1 es más inofensivo)


## In the simulation we have set up here, we know the null hypothesis is false -- the true value of difference in means is actually around 8.9. Thus, we are concerned with how often the decision rule outlined in the last section allows us to conclude that the null hypothesis is actually false. In other words, we would like to quantify the _Type II error rate_ of the test, or the probability that we fail to reject the null hypothesis when the alternative hypothesis is true.

## Unlike the Type I error rate, which we can characterize by assuming that the null hypothesis of "no difference" is true, the Type II error rate cannot be computed by assuming the alternative hypothesis alone because the alternative hypothesis alone does not specify a particular value for the difference. It thus does not nail down a specific distribution for the t-value under the alternative.

## For this reason, when we study the Type II error rate of a hypothesis testing procedure, we need to assume a particular _effect size_, or hypothetical size of the difference between population means, that we wish to target. We ask questions such as "what is the smallest difference I could reliably distinguish from 0 given my sample size N?" or, more commonly, "How big does N have to be in order to detect that the absolute value of the difference is greater than zero?" Type II error control plays a major role in designing data collection procedures **before** you actually see the data, so that you know the test you will run has enough sensitivity or _power_. Power is one minus the Type II error rate, or the probability that you will reject the null hypothesis when the alternative hypothesis is true.

## There are several aspects of a hypothesis test that affect its power for a particular effect size. Intuitively, setting a lower α decreases the power of the test for a given effect size because the null hypothesis will be more difficult to reject. This means that for an experiment with fixed parameters (i.e., with a predetermined sample size, recording mechanism, etc), the power of the hypothesis test trades off with its Type I error rate, no matter what effect size you target.

## We can explore the trade off of power and Type I error concretely using the babies data. Since we have the full population, we know what the true effect size is (about 8.93) and we can compute the power of the test for true difference between populations.

## Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets. What is the p-value (use the t-test function)?

set.seed(1)
N <- 5
dat.ns.5 <- sample(bwt.nonsmoke , N)
dat.s.5 <- sample(bwt.smoke , N)
t.test(dat.ns.5, dat.s.5)
## Welch Two Sample t-test
## data:  dat.ns.5 and dat.s.5
## t = 1.8283, df = 4.2904, p-value = 0.1366
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -10.82313  56.02313
## sample estimates:
## mean of x mean of y 
##     130.4     107.8 

set.seed(1)
get.pvalue <- function(n) {
    dat.ns <- sample(bwt.nonsmoke, n)
    dat.s <- sample(bwt.smoke, n)
    t.test(dat.s, dat.ns)$p.value < 0.05
}
rejects <- replicate(10000, get.pvalue(5))
mean(rejects)
## 0.0984

set.seed(1)
mean(replicate(10000, get.pvalue(30)))
#[1] 0.4783
mean(replicate(10000, get.pvalue(60)))
#[1] 0.7907
mean(replicate(10000, get.pvalue(90)))
#[1] 0.9338
mean(replicate(10000, get.pvalue(120)))
#[1] 0.9842

## lo mismo
Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
    set.seed(1)
    rejects <- replicate(10000,{
        dat.ns <- sample(bwt.nonsmoke , N)
        dat.s <- sample(bwt.smoke , N)
        t.test(dat.s, dat.ns)$p.value < 0.05
    })
    mean(rejects)
})
Ns[which.min(abs(res - .8))]


set.seed(1)
get.pvalue <- function(n, alpha) {
    dat.ns <- sample(bwt.nonsmoke, n)
    dat.s <- sample(bwt.smoke, n)
    t.test(dat.s, dat.ns)$p.value < alpha
}
set.seed(1)
alpha <- 0.01
mean(replicate(10000, get.pvalue(30, alpha)))
mean(replicate(10000, get.pvalue(60, alpha)))
mean(replicate(10000, get.pvalue(90, alpha)))
mean(replicate(10000, get.pvalue(120, alpha)))
