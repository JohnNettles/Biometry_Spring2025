######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 10 ######
# ANOVA
# This file contains all of the commands in the Learning the Tools section of tutorial 10. 


#### ANOVA  ####
titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)

library(ggplot2)

ggplot(titanicData, aes(x = age)) +   
  geom_histogram() + 
  facet_wrap(~ passenger_class, ncol = 1)

library(dplyr)

titanic_by_passenger_class <- group_by(titanicData,passenger_class)

summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE))

summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE), group_sd = sd(age, na.rm=TRUE))

titanicANOVA <- lm(age ~ passenger_class, data = titanicData)

anova(titanicANOVA)


#### Tukey-Kramer  ####

TukeyHSD(aov(titanicANOVA))

#### Kruskall-Wallis test  ####

kruskal.test(age ~ passenger_class, data = titanicData)
