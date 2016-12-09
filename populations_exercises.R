library(dplyr)
library(rafalib)

dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat)

## Use dplyr to create a vector x with the body weight of all males on the control (chow) diet. What is this population's average?
controlMales <- filter(dat,Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
mean(controlMales)
popsd(controlMales)
set.seed(1)
sample.x.25 <- sample(controlMales, 25)
mean(sample.x.25)

treatmentMales <- filter(dat,Sex == "M" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mean(treatmentMales)
popsd(treatmentMales)
set.seed(1)
sample.y.25 <- sample(treatmentMales, 25)
mean(sample.y.25)

abs((mean(sample.y.25) - mean(sample.x.25)) - (mean(treatmentMales) - mean(controlMales)))

## lo mismo para hembras
controlFemales <- filter(dat,Sex == "F" & Diet == "chow") %>% select(Bodyweight) %>% unlist
mean(controlFemales)
popsd(controlFemales)
set.seed(1)
sample.x.25.f <- sample(controlFemales, 25)
mean(sample.x.25.f)

treatmentFemales <- filter(dat,Sex == "F" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mean(treatmentFemales)
popsd(treatmentFemales)
set.seed(1)
sample.y.25.f <- sample(treatmentFemales, 25)
mean(sample.y.25.f)

abs((mean(sample.y.25.f) - mean(sample.x.25.f)) - (mean(treatmentFemales) - mean(controlFemales)))

## For the females, our sample estimates were closer to the population
## difference than with males. What is a possible explanation for
## this? -> The population variance of the females is smaller than
## that of the males; thus, the sample variable has less variability.

popsd(controlMales)
##[1] 4.420501
popsd(controlFemales)
##[1] 3.416438
popsd(treatmentFemales)
##[1] 5.06987
popsd(treatmentMales)
##[1] 5.574609

## --------------------------------------------------------------------------------
## minúsculas para la población, mayúscula para la muestra

## queremos comparar las medias de las 2 poblaciones (my - mx == 0?);
## in practice it would be prohibitively expensive to buy all the mice
## in a population. Here we learn how taking a sample permits us to
## answer that questin. This is the essence of statistical inference.

## podemos responder eso usando las 2 muestras (mY - mX == 0?), usando
## el Central Limit Theorem

