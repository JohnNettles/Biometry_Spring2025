######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 3 ######
# Graphics

# This file contains all of the commands in the Learning the Tools section of tutorial 3. 

library(ggplot2)

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)

# ggplot

# Histograms

ggplot(titanicData, aes(x=age)) +
  geom_histogram()


# Bar graphs

ggplot(titanicData, aes(x=sex)) +
  geom_bar(stat="count")


# Box plots

ggplot(titanicData, aes(x=sex, y=age)) + geom_boxplot()

       
# scatterplots

guppyFatherSonData <- read.csv("DataForLabs/chap02e3bGuppyFatherSonAttractiveness.csv", stringsAsFactors = TRUE)

ggplot(guppyFatherSonData, aes(x=fatherOrnamentation, y=sonAttractiveness)) +
  geom_point()

# Graphics options

ggplot(guppyFatherSonData, aes(x=fatherOrnamentation,y=sonAttractiveness)) +
  geom_point() + 
  xlab("Father's ornamentation") + ylab("Sons attractiveness")


ggplot(guppyFatherSonData, aes(x=fatherOrnamentation,y=sonAttractiveness)) +
  geom_point() + 
  xlab("Father's ornamentation") + ylab("Sons attractiveness") + 
  theme_minimal()

# getting help

help(ggplot)
