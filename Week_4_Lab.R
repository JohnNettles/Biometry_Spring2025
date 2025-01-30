library(tidyverse) #Loads dplyr, lubridate, tidyr, ggplot2, stringr

# Read in cricket data ----------------------------------------------------

data <- read.csv('Data/chapter13/chap13e5SagebrushCrickets.csv')

str(data)
head(data)
unique(data$feedingStatus)


# Visualizing data --------------------------------------------------------

ggplot(data, aes(feedingStatus, timeToMating)) +
  geom_boxplot() +
  geom_jitter(width=0.1) +
  theme_bw() +
  xlab('Feeding Status') +
  ylab('Time to Mating')



# Specify null hypothesis -------------------------------------------------

# Null = No difference in time to mating between starved and fed.
# Alternative = There is a difference in time to mating



# Specify and compute test statistic --------------------------------------------------

# Process data
st <- dplyr::filter(data, feedingStatus == 'starved')$timeToMating
fed <- dplyr::filter(data, feedingStatus == 'fed')$timeToMating

# Take the mean
mn_st <- mean(st)
mn_fed <- mean(fed)

# take difference in means
tstat <- mn_st - mn_fed



# Permute data and calculate test statistic -------------------------------
# We want to rearrange the values

# One way to do this:
# 1. Combine the data
comb <- c(st, fed)

# 2. Get indices to sample from
idx <- 1:length(comb) #vector of 1 to 24
tidx <- sample(idx,size=length(st),replace=F) #sample individuals to be starved based on # in original data

# 3. Create two new data objects
perm_1 <- comb[tidx] #all new starved individuals
perm_2 <- comb[-tidx] #all new not starved individuals

mn_perm_1 <- mean(perm_1)
mn_perm_2 <- mean(perm_2)

tstat_perm <- mn_perm_1 - mn_perm_2


# Another way to calculate it
  id <- sample(data$feedingStatus,length(comb),replace=F)
  data.frame(comb,id)
  fed_perm <- data %>% filter(id == 'fed')
  st_perm <- data %>% filter(id == 'starved')
  tstat_2 <- mean(fed_perm$timeToMating)-mean(st_perm$timeToMating)


# 4. Do it in a loop

sdis <- rep(NA,10000)
for (i in 1:10000){
  
  idx <- 1:length(comb)
  tidx <- sample(idx,size=length(st),replace=F)
  
  perm_1 <- comb[tidx]
  perm_2 <- comb[-tidx]
  
  mn_perm_1 <- mean(perm_1)
  mn_perm_2 <- mean(perm_2)
  
  tstat_perm <- mn_perm_1 - mn_perm_2
  sdis[i] <- tstat_perm
  
}

mean(sdis)

hist(sdis,
     main='Sampling distribution of test statistic',
     xlab = 'test statistic')

#plot original value from data
abline(v = tstat, col='red', lwd=3)


# Calculate p-value -------------------------------------------------------

#one-tailed test
# add 1 for each value of sdis that is less than or equal to the original test statistic. divide by # of values
sum(sdis <= tstat)/length(sdis)

# two-tailed test
plot(density(sdis), lwd = 2, ylim = c(0,0.07))
abline(v=tstat, lwd=3)
lines(density(abs(sdis)),col='red', lwd=3)
abline(v=abs(tstat),col='red', lwd=3, lty=2)




# Bootstrap ---------------------------------------------------------------

rg <- rlnorm(1000, 0, 0.5)
hist(rg)


bs_vec <- rep(NA,10000)

for (i in 1:10000){
  new_rg <- sample(rg, size=length(rg), replace=T)
  bs_vec[i] <- mean(new_rg)
}

hist(bs_vec)
sd(bs_vec)

sd(rg)/sqrt(length(rg)) #Should be same as bootstraped sd(bs_vec)

quantile(bs_vec, probs=c(0.025,0.975))

#let's say we want to calculate the 90% CI of the standard deviation
bs_vec2 <- rep(NA, 10000)
for (i in 1:10000) {
  #sample with replacement (bootstrap sample)
  new_rg2 <- sample(rg, size = length(rg), replace = TRUE)
  #take sd (rather than mean) for each bootstrap sample
  bs_vec2[i] <- sd(new_rg2)
}

#if calculating CI this way, you'll want to make sure your stat is generally symmetrical
hist(bs_vec2)
qtys2 <- quantile(bs_vec2, probs = c(0.05, 0.95))
abline(v = qtys2, lwd = 2, lty = 2, col = 'red')



# parametric bootstrap ----------------------------------------------------

#The parametric bootstrap is similar to the non-parametric bootstrap except that instead of drawing our bootstrap samples from the original data, we fit a distribution to the data first, and then draw our samples from that
# Why would we ever do a parametric bootstrap? We might use a parametric distribution if our original sample size was so small that we did not think it could “stand in” for the underlying parametric distribution. We kind of need to know the distribution the data are coming from though.

#gen data from gamma
rd <- rgamma(10, 3, 5)
hist(rd)
#get the skew of the data using the moments package
moments::skewness(rd)

#here we plot the true distribution using dgamma (just to visualize)
xv <- seq(0, 2, length.out = 100)
plot(xv, dgamma(xv, 3, 5), type = 'l', lwd = 2)

#use fitdistr to get param estimates of dist
fit <- MASS::fitdistr(rd, dgamma, list(shape = 1, rate = 1))

#loop through 10k times
bs_vec3 <- rep(NA, 10000)
for (i in 1:10000) {
  #instead of resampling from some dataset, we're drawing random values from a distribution with the same parameters as our data
  dd <- rgamma(length(rd), 
               shape = fit$estimate[1],
               rate = fit$estimate[2])
  
  #as before, we calculate our metric of interest (here skew rather than sd and mean that we used before)
  bs_vec3[i] <- moments::skewness(dd)
}

#as before we can visualize
hist(bs_vec3)

#and we can calculate the standard error of the skew estimate
sd(bs_vec)


