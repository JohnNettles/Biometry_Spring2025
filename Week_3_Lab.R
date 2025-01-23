############
#Lab Week 3
#2024-01-23
############



# Load packages -----------------------------------------------------------

#use ctrl-shift-r to create a section

install.packages('moments')
library(moments)


#For each distribution ----------------
# d = probability density function
# p = cumulative probability
# q = quantiles of distribution
# r = random numbers generated from distribution



# #rnorm - the most useful, likely ----------------------------------------

?rnorm

a <- rnorm(100, mean=0, sd=3)
rnorm(100) #mean=0, sd=1
hist(a)
mean(a) #first moment
var(a) #second moment

skewness(a) #third moment
moments::skewness(a) #helpful to specify package
moments::kurtosis(a) #fourth moment

set.seed(1) #To replicate random draws
b <- rnorm(100, mean=1, sd=1)
c <- rnorm(100, mean=1, sd=1)
b == c #Not the same since we didn't set seed in between
sum(b==c) #sum the number of trues
identical(b,c)

set.seed(1)
b2 <- rnorm(100, mean=1, sd=1)
set.seed(1)
c2 <- rnorm(100, mean=1, sd=1)
identical(b2,c2)


# dnorm -------------------------------------------------------------------

#Probability density function

?dnorm

mu <- 128 #Specify a mean
sigma <- 10 #Specify sd

#Generate sequence from 75 to 175
x <- seq(from=75, to=175, length.out=1000)

#Calculate density
#Put in x values, gives us the height of the curve
pd <- dnorm(x, mu, sigma)
length(pd)

#plot
plot(x,pd, 
     type='l', lwd=3, 
     xlab='x', ylab='Probability Density')
abline(v = mu, lwd=3, col='red') #v sets it as a vertical line



# pnorm -------------------------------------------------------------------

#Area under the pdf, up to a certain point on x axis
#Cumulative distribution function

#Weights of k-rats follow normal, mean=128, sd=10. What % will weight less than 110?

auc <- pnorm(q=110, mean=128, sd=10)

#Fill in the area under the curve
xvals <- 80:110
polygon(c(xvals,
    rev(xvals)),
  c(rep(0,length(xvals)),
    rev(dnorm(xvals,mu,sigma))),
  col='red')
text(x=80, y=0.03,
     labels = round(auc,2),
     col='red')

#Plot the cumulative density function
xvals2 <- 75:175
plot(xvals2,
     pnorm(q=xvals2, mean=mu, sd=sigma),
     type='l',
     lwd=3)
lines(xvals2,
      pnorm(q=xvals2, mean=mu-5, sd=sigma),
      lwd=3,
      col='red')
lines(xvals2,
      pnorm(q=xvals2, mean=mu-10, sd=sigma),
      lwd=3,
      col='blue')

#Similar to 'integrate' 
?integrate
integrate(f=function(x) dnorm(x,mean=128,sd=10),
          lower = -Inf,
          upper = 110)

#Another way to write function
function_name <- function(x)
{
  z <- dnorm(x,mean=128,sd=10)
  return(z)
}


# qnorm -------------------------------------------------------------------

#Quantiles of distribution
#What x value has a cumulated area of ___

?qnorm

qnorm(p=0.04, mean=mu, sd=sigma)


# Explore on your own -----------------------------------------------------

# r then histogram

data <- rpois(n=100, lambda=4)
hist(data)

# d try to plot
x <- seq(0,100)
data <- dbinom(x,40,0.4)

plot(x,data,
     type='l',
     lwd=3)
  
# p get AUC for specified values

auc <- pbeta(q=0.3,2,3)
x <- seq(0,1,length.out=100)
plot(x,dbeta(x,2,3))


xvals <- seq(0,0.3,length=100)
polygon(c(xvals,
          rev(xvals)),
        c(rep(0,length(xvals)),
          rev(dbeta(xvals,2,3))),
        col='red')
text(x=80, y=0.03,
     labels = round(auc,2),
     col='red')

# q get value where AUC is 0.5



# Sample from poisson -----------------------------------------------------



draw <- function(N) #Specify default
{
  mns <- rep(NA,1000)
  
  for(i in 1:1000){
    print(i)  #see how far it has gotten
    tt <- rpois(N,lambda=4)
    mns[i] <- mean(tt)
  }
  return(mns)
}

str(mns)
hist(mns) #Sample distribution of the means
sd(mns) #standard error of the mean

n1 <- draw(N=1)
n10 <- draw(N=10)
n100 <- draw(N=100)
n1000<- draw(N=1000)
hist(n1,xlim=c(0,15))
hist(n10,xlim=c(0,15))
hist(n100,xlim=c(0,15))
hist(n1000,xlim=c(0,15))
