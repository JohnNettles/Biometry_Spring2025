######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 7 ######
# Contingency analysis
# This file contains all of the commands in the Learning the Tools section of tutorial 7. 


titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)
#### Factors

levels(titanicData$survive)
titanicData$survive <- factor(titanicData$survive, levels = c("yes", "no"))


####  Frequency tables ####

sex_survive_table <- table(titanicData$sex, titanicData$survive)

sex_survive_table

sex_survive_table_direct <- data.frame(yes = c(307,142), no = c(156,708), 
  row.names = c("female","male"))

####  Mosaic pots ####
mosaicplot(sex_survive_table)
mosaicplot(sex_survive_table, color = c("darkred","gold"), xlab  ="Sex", ylab = "Survival")

#### Odds ratios ####
sex_survive_fisher = fisher.test(sex_survive_table)
sex_survive_fisher$estimate
sex_survive_fisher$conf.int


#### Chi-squared contingency analysis ####
chisq.test(sex_survive_table)$expected
chisq.test(sex_survive_table, correct=FALSE)

#### Fisher's exact test ####

fisher.test(sex_survive_table)


##### Question 5  #####


tCountTable = data.frame(Common = c (25, 4), Uncommon = c(55, 10), row.names = c("Found", "Not found"))
