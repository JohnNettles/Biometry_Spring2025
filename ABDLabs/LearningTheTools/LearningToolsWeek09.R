######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 9 ######
# COMPARING MEANS OF TWO GROUPS
# This file contains all of the commands in the Learning the Tools section of tutorial 9. 


titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)
library(ggplot2)

#### Stripchart ####
ggplot(titanicData, aes(x = survive, y = age)) + 
  geom_jitter(position = position_jitter(0.05)) +
  theme_minimal()


# multiple histograms
ggplot(titanicData, aes(x = age)) +   
         geom_histogram() + 
         facet_wrap(~ survive, ncol = 1) 

#violin plots
ggplot(titanicData, aes(x=survive, y=age, fill = survive)) + 
  geom_violin() +
  xlab("Survival") + ylab("Age") + 
  theme_classic()+scale_fill_manual(values=c("#FFB531","#BC211A"))+ 
  stat_summary(fun.y=mean,  geom="point", color="black")+ 
  theme(legend.position="none")+ 
  theme(aspect.ratio=1)

       
####  2-sample t-test

t.test(age ~ survive, data = titanicData, var.equal = TRUE)


####  Welch's t-test

t.test(age ~ survive, data = titanicData, var.equal = FALSE)


##### Paired t-test ####

blackbird <- read.csv("DataForLabs/chap12e2BlackbirdTestosterone.csv")
  
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired = TRUE)


#### Levene's test ####

library(car)

leveneTest(data = titanicData, age ~ survive, center = mean)
