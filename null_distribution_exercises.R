x <- unlist(read.csv("femaleControlsPopulation.csv"))

## Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages. What proportion of these 1,000 averages are more than 1 gram away from the average of x ?

set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
## hist(averages5)
mean(abs(averages5 - mean(x)) > 1)

set.seed(1)
n <- 10000
averages5.10k <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5.10k[i] <- mean(X)
}
## hist(averages5.10k)
mean(abs(averages5.10k - mean(x)) > 1)

## una random distribution tiene los valores que obtendríamos si hiciéramos el experimento infinitas veces. aumentar el nro de experimentos no cambia la distribución, como se ve al pasar de 1000 a 10000.

## pero si cambiamos el tamaño de la muestra, cambia la variable aleatoria, y eso cambia la distribución

set.seed(1)
n <- 1000
averages50.1k <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50.1k[i] <- mean(X)
}
## hist(averages50.1k)
mean(abs(averages50.1k - mean(x)) > 1)
