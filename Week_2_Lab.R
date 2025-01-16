#Using seq
seq(1,5) #gives just integer values

seq(1,5, length.out=100) #100 values from 1 to 5

seq(2, 10, by=2) #count by twos

#Other ways to create vector
c(1:5)
1:5
vec <- c(1,3,7,4)
str(vec) #defines structure of our 'vec' object
class(vec)
typeof(vec)

#Vectorized operations
vec[1]*2
vec[2]*2
vec[3]*2
vec[4]*2

vec*2 #Does it for all values in vector

#sort/rank/rev
sort(vec)
sort(vec,decreasing=T)
rev(sort(vec, decreasing=T)) #rev() reverses the argument
rank(vec) #returns rank of each value (1st, 2nd, etc.)

#Indexing and excluding
vec[1:3] #positions 1 through 3
vec[c(1,3)] #just want positions 1 and 3
vec[-1] #exclude the first value

#Data Frames: object in which the various columns may be of different type
#Setting up or reading in a df

dd <- read.csv("ABDLabs/DataForLabs/studentSampleData.csv")

dd$height_cm
dd[,2] #Just want column 2
dd[c(1,10),]
dd$height_cm == dd[,2]
identical(dd$height_cm,dd[,2])


#Matrix
#Every element has to have the same type
mat <- matrix(NA, nrow=4, ncol=4)

#Indexing matrices
mat[1,1] <- 6
mat
mat[3,2] <- 'g' #Won't be able to do anything with it because its not all the same type
mat

#Matrix operations (?matmult)
x <- 1:4  #Vector from 1 to 4
(z <- x %*% x)    # scalar ("inner") product (1 x 1 matrix) 
            #adding parentheses will assign value and then print it
str(z)
drop(z)             # as scalar
z2 <- drop(z)


y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
(y %*% z)
y %*% x
x %*% z

diag(5) #creates a matrix of zeros with 1 down the diagonal
class(z)


#Array
#Like matrix but can be more dimensions
aa <- array(1:3, c(2,4,3))

#indexing
aa[1,2,3]
aa







