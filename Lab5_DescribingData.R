titanicData <- read.csv("ABDLabs/DataForLabs/titanic.csv",stringsAsFactors = T)
titanicData$age

mean(titanicData$age)
# [1] NA

mean(titanicData$age, na.rm=T)
# [1] 31.19418

median(titanicData$age, na.rm=T)
var(titanicData$age, na.rm=T) #calculates variance
summary(titanicData$age, na.rm=T)
sd(titanicData$age, na.rm=T)

#Coefficient of variation
100*sd(titanicData$age, na.rm=T)/mean(titanicData$age, na.rm=T)

IQR(titanicData$age, na.rm=T) #Interquartile range

#Confidence intervals of the mean
t.test(titanicData$age)$conf.int
t.test(titanicData$age, conf.level=0.99)$conf.int
