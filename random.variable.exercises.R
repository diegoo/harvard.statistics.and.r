library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist(read.csv(filename))

## Here x represents the weights for the entire population.

mean(x)
## 23.89338

set.seed(1)
abs(mean(x) - mean(sample(x, 5)))
## 0.2706222

set.seed(5)
abs(mean(x) - mean(sample(x, 5)))
## 1.433378