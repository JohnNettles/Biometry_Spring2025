######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 5 ######
# Describing data
# This file contains all of the commands in the Learning the Tools section of tutorial 5. 



# Missing data

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)
titanicData$age

#mean
mean(titanicData$age)

mean(titanicData$age, na.rm = TRUE)

#median and summary
median(titanicData$age, na.rm = TRUE)

summary(titanicData$age)

# Measures of variability

var(titanicData$age, na.rm = TRUE)
sd(titanicData$age, na.rm = TRUE)

sqrt(var(titanicData$age, na.rm = TRUE))

# Coefficient of variation

100 * sd(titanicData$age, na.rm = TRUE) / mean(titanicData$age, na.rm = TRUE)

# Interquartile range

IQR(titanicData$age, na.rm = TRUE)

# Confidence interval for the mean

t.test(titanicData$age)$conf.int
t.test(titanicData$age, conf.level = 0.99)$conf.int

# Optional -- calculating the Standard error of the mean

SEM = function(datalist){sd(datalist, na.rm = TRUE) / sqrt(sum(!is.na(datalist)))}

SEM(titanicData$age)



#  Quantiles

quantile(titanicData$age, c(0.025, 0.975), na.rm = TRUE)
