library(gapminder)
data(gapminder)
head(gapminder)

## Create a vector 'x' of the life expectancies of each country for the year 1952. Plot a histogram of these life expectancies to see the spread of the different countries.

x <- gapminder$lifeExp
hist(x)

## In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.
## We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.
## The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.
## Let's continue, using the simpler, mean() function.
## What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?

data.1952 <- subset(gapminder, year == 1952)
mean(data.1952$lifeExp <= 40)

## lifeExp entre 40 y 60
mean(data.1952$lifeExp <= 60) - mean(data.1952$lifeExp <= 40)

## para ver la proporción de registros que tienen una lifeExp dada:
## plot(ecdf(x))
##
## lo mismo:

proportion = function(q) { mean(x <= q) }
quantities = seq(from=min(x), to=max(x), length=20)
proportions = sapply(quantities, proportion)
plot(quantities, proportions)
## o
proportions = = sapply(quantities, function(q) mean(x <= q))
## plot(ecdf(x))


