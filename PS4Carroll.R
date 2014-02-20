#Tommy Carroll
#Problem Set 4

#Problem: Read in NetLogo File
setwd("C:/Users/Thomas/Downloads")

temporary <- scan(file="NetLogo.csv", what=" ", sep=",") 
#create object 'temporary', which contains every element of the NetLogo csv. This makes it easier to figure out 
#how many lines we should skip to find where to begin extracting information. 

output <- vector("list") #create object we will store the results of the NetLogo model in

#GLOBALS
# First: read in column names of the Globals
globals.skip <- (which(temporary=="GLOBALS") - 1)/84 + 1 #calculates number of lines to skip before reading
                                                          #in Globals info. We use 84 because this NetLogo
                                                          #file has 84 columns in it; therefore the 85th element
                                                          #of temporary is the first element of the second row; 
                                                          #the 169th element is the first element of the 3rd row; 
                                                          #etc. We subtract one to make it evenly divisible by 84; 
                                                          #then we add 1 to make sure we don't scan in the line that
                                                          #contains headings like the word 'GLOBALS'
global.colnames <- scan(file="NetLogo.csv", skip=globals.skip, nlines=1, what=" ", sep=",") #scan in the column names for the globals

# Second: read in the actual values for the different Global parameters. These are stored on the line
#         right after the column names, so we simply skip the same number of lines as we did in the 
#         last step, plus 1. 
global.values <-  scan(file="NetLogo.csv", skip=globals.skip+1, nlines=1, what=" ", sep=",")

# Third: clean up the global.values object by getting rid of brackets 
temporary <- global.values
temporary <- gsub("\\[", "", temporary)
temporary <- gsub("\\]", "", temporary)

# Fourth: separate each column that has a vector for a value into multiple columns, so that each column has
# only one value. 
temporary2 <- strsplit(temporary, split=" ") #takes temporary and turns it into a list, where every element of the list
                                            #is a vector containing every separate element of that column
global.length.temp <- numeric(length=length(temporary2)) #create empty numeric vector
for(i in 1:length(temporary2)){ 
  #create a vector that shows how many elements are in each column of the NetLogo file
  global.length.temp[i] <- length(temporary2[[i]])
}
global.colnames2 <- rep(global.colnames, global.length.temp) #create a new vector of column names; repeat each column name by the number
                                        #of elements that were in that column

#The for loop below simply takes the new vector of column names and adds numbers to those column names that repeat
#This allows us to differentiate between different global parameter values that used to be in the same vector
for(i in 1:length(global.colnames2)){
  integers <- NULL
  n <- sum(global.colnames2[i]==global.colnames2)
  if(n > 1){
    integers <- 1:n  
    global.colnames2[which(global.colnames2[i]==global.colnames2)] <- 
      paste(global.colnames2[which(global.colnames2[i]==global.colnames2)], "(", integers, ")", sep="")
  }
}

#As an example to help illustrate what I just did: assume the NetLogo file had a global parameter named 'testing' and 
#in the NetLogo file this testing parameter was a vector 3 units long. Then the new objects I've created will
#actually have three columns respectively labeled 'testing', 'testing(2)', and 'testing(3)', and each of these columns
#will have one of the three elements of the vector from the original NetLogo file. 

#Finally, we use the unlist command on temporary2 to get every parameter from the global row list as its own
#element in a vector. Essentially this breaks up vectors in the NetLogo file, so that columns that had vector elements
#no longer exist; instead every column has only one element in it, no vectors. 
global.values2 <- unlist(temporary2)

# Fifth: create an empty list, called "globals"; each element of this list will be one of the global parameters
globals <- vector("list")
for (i in 1:length(global.colnames2)){
  globals[[i]] <- global.values2[i]
}
names(globals) <- global.colnames2
output$globals <- globals #store the list of global parameters as the first element in the output list



#TURTLES

temporary <- as.character(turtle.data[,36])
temporary <- head(gsub("\\[", "", temporary))
temporary <- head(gsub("\\]", "", temporary))
temporary



turtles <- scan(file="NetLogo.csv", skip=13, nlines=4786, what=" ", sep=",")
turtles.mat <- matrix(turtles, nrow=4786, byrow=TRUE)
turtles.mat <- turtles.mat[,-c(39:84)]
turtle.names <- scan(file="NetLogo.csv", skip=12, n=38, what=" ", sep=",")
turtle.data <- data.frame(turtles.mat)
colnames(turtle.data) <- turtle.names

activist.data <- subset(turtle.data, breed=="{breed activists}")
cands.data <- subset(turtle.data, breed=="{breed cands}")
districts.data <- subset(turtle.data, breed=="{breed districts}")
parties.data <- subset(turtle.data, breed=="{breed parties}")
voter.data <- subset(turtle.data, breed=="{breed voters}")
output$activist.data <- activist.data
output$cands.data <- cands.data
output$districts.data <- districts.data
output$parties.data <- parties.data
output$voter.data <- voter.data
head(output$activist.data)
class(output$activist.data)
## Check constant values:
# shape
unique(activist.data$shape);unique(cands.data$shape);unique(districts.data$shape);unique(parties.data$shape);unique(voter.data$shape)

# label
unique(activist.data$label);unique(cands.data$label);unique(districts.data$label);unique(parties.data$label);unique(voter.data$label)
# 
unique(activist.data$label);unique(cands.data$label);unique(districts.data$label);unique(parties.data$label);unique(voter.data$label)


## Save the output
write.table(output$globals, file="~/Dropbox/2014 Spring/Programming/globals.txt")
write.csv(output$acvitist.data, file="~/Dropbox/2014 Spring/Programming/acvitist.csv")











####JMR####
####Chapter 4, Problem 3####

problem4.3 <- function(n){
  number <- 1:n #create vector of the actual numbers
  square <- number^2 #compute vector of the square of each number
  cube <- number^3  #compute vector of the cube of each number
  temp <- cbind(number, square, cube) 
  write.table(x=temp, file="square_cube.txt", append=FALSE) #column bind the vectors into a single table
  show(temp)
}
problem4.3(7) #example of the code running


####Chapter 4, Problem 4####
#create.matrix function takes 2 arguments: n is number of rows, k is number of columns. The function 
#allows us to make a multiplcation table of any size. 
create.matrix <- function(n, k){
  mtable <- matrix(0, nrow=n, ncol=k) #create matrix of all 0s
  for(i in 1:n){ #for() loop plugs each row into the matrix 
    mtable[i,] <- i*(1:k)
  }
  write(mtable, file="Multiplication_Table.txt", sep=" ", append=FALSE, ncol=k) #writes the table to a file
  show(mtable)
}

create.matrix(9,9) #example of code used to make 9x9 standard multiplication table

####Chapter 7, Problem 3####
library(lattice) #load lattice library

#next few lines are just copied straight from book; used to generate the initial random set of heights
pop <- data.frame(m=rnorm(100, 160, 20), f=rnorm(100, 160, 20))
male.heights <- data.frame(height=pop$m, generation=factor(rep(1, 100), levels=c(1:9)))

next.gen <- function(pop){
  pop$m <- sample(pop$m)
  pop$m <- apply(pop, 1, mean)
  pop$f <- pop$m
  return(pop)
}

#Next run a for loop to generate the 9 generations worth of data; plug into a 900x2 dataframe, sorted by generation
for(i in 2:9){
  pop <- next.gen(pop=pop)
  temp.data <- data.frame(height=pop$m, generation=factor(rep(i, 100), levels=c(1:9)))
  male.heights <- rbind(male.heights, temp.data)
}

#Finally use lattice package to create a 3x3 graph of histograms showing how every individual's height converges
#towards the mean of 160
histogram(~height | generation, data=male.heights, xlab="Height", main="Distribution of Male Height by Generation",
          as.table=TRUE) #The index.cond argument allows me to set the order
                                                        #of the panels
#index.cond=list(c(7, 8, 9, 4, 5, 6, 1, 2, 3)) --- this argument can be used to specifically set what order the panels appear in

####Chapter 7, Problem 4####
library(spuRs)
data(treeg) #read in data

xyplot(height.ft ~ age | tree.ID, data=treeg, type="l", xlab="Age", ylab="Height (in feet)", as.table=TRUE)
