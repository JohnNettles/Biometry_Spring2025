######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 11 ######
# Correlation and regression
# This file contains all of the commands in the Learning the Tools section of tutorial 11. 

library(ggplot2)

guppyData <- read.csv("DataForLabs/chap02e3bGuppyFatherSonAttractiveness.csv", stringsAsFactors = TRUE)

ggplot(guppyData, aes(x=fatherOrnamentation, y=sonAttractiveness)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness") 

#### Correlation  ####

cor(guppyData$fatherOrnamentation, guppyData$sonAttractiveness)

cor(guppyData$fatherOrnamentation, guppyData$sonAttractiveness)^2

cor.test(guppyData$fatherOrnamentation, guppyData$sonAttractiveness)

#### Spearman's correlation  ####

cor.test(guppyData$fatherOrnamentation, guppyData$sonAttractiveness, method = "spearman")



#### Linear regression  ####

guppyRegression <- lm(sonAttractiveness ~ fatherOrnamentation, data = guppyData)

guppyRegression 

summary(guppyRegression)

ggplot(guppyData, aes(x=fatherOrnamentation, y=sonAttractiveness)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness") +    
  geom_smooth(method = lm)

ggplot(guppyData, aes(x=fatherOrnamentation, y=sonAttractiveness)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness") +    
  geom_smooth(method = lm, se = FALSE)

#Residuals

residuals(guppyRegression)

plot(guppyData$fatherOrnamentation, residuals(guppyRegression))
abline(h=0)

