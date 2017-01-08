d <- read.csv("assoctest.csv")

## This dataframe reflects the allele status (either AA/Aa or aa) and the case/control status for 72 individuals.

## Compute the Chi-square test for the association of genotype with
## case/control status (using the table() function and the
## chisq.test() function). Examine the table to see if it look
## enriched for association by eye. What is the X-squared statistic?

chisq.test(table(d))
##         Pearson's Chi-squared test with Yates' continuity correction
## data:  table(d)
## X-squared = 3.3437, df = 1, p-value = 0.06746

## lo mismo

tab = table(d$allele, d$case)
chisq.test(tab)

## Compute the Fisher's exact test ( fisher.test() ) for the same table. What is the p-value?

fisher.test(table(d))
##         Fisher's Exact Test for Count Data
## data:  table(d)
## p-value = 0.05194
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.940442 8.493001
## sample estimates:
## odds ratio 
##   2.758532

## lo mismo

tab = table(d$allele, d$case)
fisher.test(tab)
