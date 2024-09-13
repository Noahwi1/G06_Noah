#Analysis
install.packages("pwr")
library(pwr)
#test different power analysis for significance level 0.1, 0.3 and 0.5
pwr.r.test(n = 800, r = 0.5, sig.level = 0.05, power = NULL,
           alternative = c("two.sided", "less", "greater"))
#for 0.1 -> pwr = 0.809, for 0.3 -> pwr = 1, for 0.5 -> pwr = 1

#in a next step, run 10 analyses with sig.level = 0.005
pwr.r.test(n = 800, r = 0.5, sig.level = 0.005, power = NULL,
           alternative = c("two.sided", "less", "greater"))
#for 0.1 -> pwr = 0.51, for 0.3 -> pwr = 1, for 0.5 -> pwr = 1

#What is the minimum sample size you need to replicate a large, medium, or a small effect with a power of 90%?
pwr.r.test(n = NULL, r = 0.5, sig.level = 0.005, power = 0.9,
           alternative = c("two.sided", "less", "greater"))
#for 0.1 -> N = 1663.06, for 0.3 -> N = 177.1, for 0.5 -> N = 58.1

#Your replication sample contains about 250 participants with high-quality data.
#How much power do you have to replicate a significant finding for a small, 
#medium or large effect (alpha=0.05, two-sided) in this sample?
pwr.r.test(n = 250, r = 0.5, sig.level = 0.05, power = NULL,
           alternative = c("two.sided", "less", "greater"))
#for 0.1 -> pwr = 0.35, for 0.998 -> pwr = 1, for 0.5 -> pwr = 1

#Exercise 4

mydata <- read.table(file.choose(), header=T, sep="\t")

View(mydata)
dim(mydata)
summary(mydata)
head(mydata)

#There is a column «Filter», delete all participants with the entry «1».
#How many participants are left after removing all with Filter = 1?
mydata$Sex <- as.factor(mydata$Sex)
mydata$Filter <- as.factor(mydata$Filter)
summary(mydata)

mydata <- subset(mydata, Filter == 0)
nrow(mydata)
#833 participants are left after removing low quality data
           