######## Analysis of Biological Data Labs -- Learning the tools --  Tutorial 4  #######

#  Sampling


# Let's write a function to speed up the Week 3 lab, that takes a data frame
# with some number of shell measurements and:
#    (1) Sample x individuals by row and display
#    (2) Display the meansures for those rows
#    (3) Calculate the mean of those measures


length=seq(1:30)
population_data = as.data.frame(length) ### Replace with your real data!

# For the following function "mean_from_a_sample", datalist should be a column
# that gives a list of a single numerical variable over a number of individuals.
# This will represent the population from which the sample is taken.

# sample_size is the number of individuals that you want to
# have in the sample

mean_from_a_sample <- function(datalist,sample_size) {
  population_size = length(datalist)  # Establish the size of the "population"
  sample_numbers = sample.int(population_size,sample_size)  # chooses which individuals to sample
  sample_values = datalist[sample_numbers]  # Looks up the values for those individuals in the data
  sample_mean = mean(sample_values,na.rm=TRUE)   # Calculates the mean of thoise sampled individuals
  sample_mean
}


#####   Copy the above function all the way to here to paste into the console.

##### Taking 1000 samples and listing the mean of each sample

sample_mean_length = rep(0,1000)
for(i in 1:1000) sample_mean_length[i]= mean_from_a_sample(population_data$length,5)

##Calculating the mean of all sample means

mean(sample_mean_length)
