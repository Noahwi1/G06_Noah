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

#Quality: There is a column «Filter», delete all participants with the entry «1».
#How many participants are left after removing all with Filter = 1?
mydata$Sex <- as.factor(mydata$Sex)
mydata$Filter <- as.factor(mydata$Filter)
summary(mydata)

mydata <- subset(mydata, Filter == 0)
nrow(mydata)
#833 participants are left after removing low quality data

#Validation: If you look at the testosterone values, does the differences
#between the two sex groups makes sense? Which group are males, which group are
#females? Do you see the same pattern in the smaller replication sample?
t.test(mydata$Testosteron ~ mydata$Sex)
plot(mydata$Testosteron ~ mydata$Sex)
summary(mydata$Testosteron[mydata$Sex==0])
summary(mydata$Testosteron[mydata$Sex==1])
#mean in group 0 = 1.22, mean in group 1 = 23.2 (0 = fem, 1 = male)
mydata$Sex_ch <- factor(mydata$Sex, levels=c(0,1), labels=c("female", "male"))
summary(mydata)
summary(lm(Extraversion ~ Sex, data=mydata))

#repdata:
repdata <- read.table(file.choose(), header=T, sep="\t")

View(repdata)
dim(repdata)
summary(repdata)
head(repdata)

#There is a column «Filter», delete all participants with the entry «1».
#How many participants are left after removing all with Filter = 1?
repdata$Sex <- as.factor(repdata$Sex)
repdata$Filter <- as.factor(repdata$Filter)
summary(repdata)

repdata <- subset(repdata, Filter == 0)
nrow(repdata)

t.test(repdata$Testosteron ~ repdata$Sex)
plot(repdata$Testosteron ~ repdata$Sex)
summary(repdata$Testosteron[repdata$Sex==0])
summary(repdata$Testosteron[repdata$Sex==1])


repdata$Sex_ch <- factor(repdata$Sex, levels=c(0,1), labels=c("female", "male"))
summary(repdata)


#Reliability: For the memory data, you have a repeated measurement (SD = short
#delay, after 10 min & LD = long delay – after 1 day). How high is the correlation
#between these two measurements? Is there a performance difference between SD and LD?
#Does this make sense?
cor(mydata$EM_SD, mydata$EM_LD, use="pairwise")
t.test(mydata$EM_SD, mydata$EM_LD)
summary(mydata$EM_SD)
summary(mydata$EM_LD)
#correlation between these measurements is 0.857, and the performance difference 
#is significant with a p-value = 2.2e-16. It makes sense that there is not a perfect 
#but good correlation, because of the 1 day delay. The performance difference is also
#to be expected. 

#repdata:
cor(repdata$EM_SD, repdata$EM_LD, use="pairwise") #Correlation: 0.8684151
t.test(repdata$EM_SD, repdata$EM_LD) # p-value = 4.475e-07
summary(repdata$EM_SD) #mean 28.17
summary(repdata$EM_LD) #mean 23.46 > showing the same pattern


#Aggregation: Since the SD and LD memory performance are highly correlated, 
#you can calculate also the average memory performance for the downstream analysis 
#for both samples.
mydata$EM <- (mydata$EM_SD + mydata$EM_LD)/2
plot(density(mydata$EM_SD), main="Compare EM SD and LD", frame.plot=F)
lines(density(mydata$EM_LD), col="red")
lines(density(mydata$EM), col="green")

#repdata
repdata$EM <- (repdata$EM_SD + repdata$EM_LD)/2
plot(density(repdata$EM_SD), main="Compare EM SD and LD", frame.plot=F)
lines(density(repdata$EM_LD), col="red")
lines(density(repdata$EM), col="green")

#Validation: There is fMRI-data available from the hippocampus and amygdala, 
#comparing the fMRI signal between negative and neutral pictures. The two brain 
#regions should be closely related. How high is the correlation between the two 
#brain signals? Does this make sense? Is it the same in both samples?
cor(mydata$fMRI_amy_neg_neu, mydata$fMRI_hipp_neg_neu)
plot(mydata$fMRI_amy_neg_neu, mydata$fMRI_hipp_neg_neu, pch=19)
abline(lm(mydata$fMRI_hipp_neg_neu ~ mydata$fMRI_amy_neg_neu))
summary(lm(EM ~ Sex + Extraversion + fMRI_hipp_neg_neu, data=mydata))
#correlation is 0.75

#repdata
cor(repdata$fMRI_amy_neg_neu, repdata$fMRI_hipp_neg_neu) #Corr. 0.7660941
plot(repdata$fMRI_amy_neg_neu, repdata$fMRI_hipp_neg_neu, pch=19)
abline(lm(repdata$fMRI_hipp_neg_neu ~ repdata$fMRI_amy_neg_neu))

summary(lm(EM ~ Sex_ch + Extraversion + fMRI_hipp_neg_neu, data=repdata))
#Same as mydata

#Statistical analysis: After finishing your validation and reliability checks, 
#filtering of the data and also the building of new (averaged) behavioral data, 
#you start to think about a hypothesis you can test and replicate in your datasets.
#Add an appropriate statistical analysis to your script and run and replicate this analysis.
#Hypothesis: Extrav and WM correlate positively
correlation <- cor(mydata$EM_SD, mydata$Extraversion) #corr. = 0.0256
plot(mydata$EM_SD ~ mydata$Extraversion)
cor.test(mydata$EM_SD, mydata$Extraversion)


#Filter Part 1: potentially separating the power analysis from the quality checks, 
#and combining reliability, validity and objectivity in one script. 




           