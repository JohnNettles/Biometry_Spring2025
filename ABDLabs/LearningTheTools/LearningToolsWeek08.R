######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 8 ######
# Normal distribution and sample means
# This file contains all of the commands in the Learning the Tools section of tutorial 8. 

library(ggplot2)

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)

#  Histograms

ggplot(titanicData, aes(x = age)) + geom_histogram(binwidth = 10)

hist(titanicData$age)

# QQ plots
qqnorm(titanicData$age)
qqline(titanicData$age)

# Random numbers from a normal distribution

rnorm(n = 20, mean = 13, sd = 4)

normal_vector <- rnorm(n = 100, mean = 13, sd = 4)
qqnorm(normal_vector)
qqline(normal_vector)

#  QQ plots for a non-normal disitrbution

countries <- read.csv("DataForLabs/countries.csv", stringsAsFactors = TRUE)
popSizes = countries$total_population_in_thousands_2015
hist(popSizes, breaks = 10)
qqnorm(popSizes)
qqline(popSizes)

# QQ plot from non-normal distribution

log(titanicData$age)
