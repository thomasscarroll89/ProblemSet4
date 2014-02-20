#Tommy Carroll
#Problem Set 4

#Problem: Read in NetLogo File
setwd("C:/Users/Thomas/Downloads")
temporary <- scan(file="NetLogo.csv", what=" ", sep=",") 
#create object 'temporary', which contains every element of the NetLogo csv. This makes it easier to figure out 
#how many lines we should skip to find where to begin extracting information. 

####MAIN FILE NAME####
#Start creating the main file name. Scan in the 2nd and 3rd lines of the file, and select only the first and 85th
# elements of the vector (these correspond to the first element of each row in the csv file...which are the only
# elements that contain any info. All the rest are empty)
main.file.name <- scan(file="NetLogo.csv", skip=1, nlines=2, what=" ", sep=",")[c(1, 85)] 
main.file.name <- unlist(strsplit(main.file.name, split=" "))[-4] #use -4 to drop the 4th element, which contains some info that 
  # I don't understand, but it doesn't seem to be related to date or time
main.file.name <- paste(main.file.name[1], main.file.name[2], main.file.name[3], sep=".")
main.file.name <- gsub(":", "-", main.file.name) #use gsub to get rid of illegal characters in file name
main.file.name <- gsub("/", "-", main.file.name)

####GLOBALS####
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
temp <- global.values
temp <- gsub("\\[", "", temp)
temp <- gsub("\\]", "", temp)

# Fourth: separate each column that has a vector for a value into multiple columns, so that each column has
# only one value. 
temporary2 <- strsplit(temp, split=" ") #takes temp and turns it into a list, where every element of the list
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

####TURTLES####
# First: decide how many lines to read in, and where to start reading in. Use the same process that was used in 
# GLOBALS section above
turtles.skip <- (which(temporary=="TURTLES") - 1)/84 + 1 #how many lines to skip
turtle.names <- scan(file="NetLogo.csv", skip=turtles.skip, nlines=1, what=" ", sep=",") #read in turtle column names
turtle.names <- turtle.names[turtle.names!=""] #get rid of missing column names

turtles.end <- (which(temporary=="PATCHES") - 1)/84 - 1 #calculate when to stop scanning in data for turles
turtle.lines2read <- turtles.end - (turtles.skip + 1) #calculates number of lines to read, by subtracting when
                                                      #Turtles section begins from when it ends

#Now read in the data itself
turtles <- scan(file="NetLogo.csv", skip=(turtles.skip+1), nlines=turtle.lines2read, what=" ", sep=",")
  #scan in the turtle data

#Next clean up the data a little by removing brackets and curly brackets
turtles <- gsub("\\[", "", temp)
turtles <- gsub("\\]", "", temp)
turtles <- gsub("\\{", "", temp)
turtles <- gsub("\\}", "", temp)

turtles.mat <- matrix(turtles, nrow=turtle.lines2read, byrow=TRUE) #convert data to matrix format

#Following for() loop cleans up turtles matrix by getting rid of any columns that contain only empty values
for(i in ncol(turtles.mat):1){
  if(all(turtles.mat[,i]=="")){ #If statement tests to see if each column contains only missing values; if it does
    #the next line of code drops that column
    turtles.mat <- turtles.mat[,-i]
  }
}

# Second: We're going to subset this matrix into 5 different categories: Districts, Voters, Activists,
#  Parties, and Candidates
districts <- turtles.mat[turtles.mat[,9]=="breed districts",]
voters <- turtles.mat[turtles.mat[,9]=="breed voters",]
activists <- turtles.mat[turtles.mat[,9]=="breed activists",]
parties <- turtles.mat[turtles.mat[,9]=="breed parties",]
candidates <- turtles.mat[turtles.mat[,9]=="breed cands",]

# Third: We're going to do something similar to what we did in the GLOBALS section above to take any columns with vectors
# in them and make multiple columns, where each column contains one of the values of that vector. We'll do this 
# for all 5 subsetted matrices. 

adding.columns <- function(DATA, originalColumnNames){
  row1 <- DATA[1,]
  temporary2 <- strsplit(row1, split=" ")
  turtles.length.temp <- numeric(length=length(temporary2))
  for(i in 1:length(temporary2)){
    turtles.length.temp[i] <- length(temporary2[[i]])
    if(turtles.length.temp[i]==0){ #Some columns are empty, and so the value for length(temporary2[[i]])
                                    # is 0. We add 1 to these observations so that we still repeat the column
                                    # name at least once. 
      turtles.length.temp[i] <- 1
    }
  }
  dataColNames <- rep(originalColumnNames, turtles.length.temp)
  #The for loop below simply takes the new vector of column names and adds numbers to those column names that repeat
  #This allows us to differentiate between different global parameter values that used to be in the same vector
  for(i in 1:length(dataColNames)){
    integers <- NULL
    n <- sum(dataColNames[i]==dataColNames)
    if(n > 1){
      integers <- 1:n  
      dataColNames[which(dataColNames[i]==dataColNames)] <- 
        paste(dataColNames[which(dataColNames[i]==dataColNames)], "(", integers, ")", sep="")
    }
  }
  return(dataColNames)
}

districts.colnames <- adding.columns(DATA=districts, turtle.names)
voters.colnames <- adding.columns(DATA=voters, turtle.names)
activists.colnames <- adding.columns(DATA=activists, originalColumnNames=turtle.names)
parties.colnames <- adding.columns(DATA=parties, turtle.names)
candidates.colnames <- adding.columns(DATA=candidates, turtle.names)

#Finally, we create another function to go through and extend each vector into its separate elements. 
adding.columns2 <- function(DATA, columnNumber=length(districts.colnames)){
  #First we need to replace missing values with NAs
  for(i in 1:nrow(DATA)){
    DATA[i,which(DATA[i,]=="")] <- NA
  }
  #Next we use strsplit and unlist to get each individual element; then we turn these into a matrix
  temporary2 <- strsplit(DATA, split=" ")
  turtle.values <- unlist(temporary2)
  turtle.mat <- matrix(turtle.values, ncol=columnNumber)
  return(turtle.mat)
}

#Then we run this function on each of the 5 subsets of the TURTLE data. We also convert each resulting matrix 
# into a dataframe and add the relevant column names. 
districts.data <- data.frame(adding.columns2(DATA=districts, length(districts.colnames)))
voters.data <- data.frame(adding.columns2(DATA=voters, length(voters.colnames)))
activists.data <- data.frame(adding.columns2(DATA=activists, length(activists.colnames)))
parties.data <- data.frame(adding.columns2(DATA=parties, length(parties.colnames)))
candidates.data <- data.frame(adding.columns2(DATA=candidates, length(candidates.colnames)))

colnames(districts.data) <- districts.colnames
colnames(voters.data) <- voters.colnames
colnames(activists.data) <- activists.colnames
colnames(parties.data) <- parties.colnames
colnames(candidates.data) <- candidates.colnames

#Next, we go through all five datasets and drop any irrelevant columns (i.e. any columns that have all missing data)
#Start by creating a function that drops any column with all missing values
drop.missing.columns <- function(dataset){
  for(i in ncol(dataset):1){
    if(all(is.na(dataset[,i]))){ #If statement tests to see if each column contains only missing values; if it does
      #the next line of code drops that column
      dataset <- dataset[,-i]
    }
  }
  return(dataset)
}
districts.data <- drop.missing.columns(districts.data)
voters.data <- drop.missing.columns(voters.data)
activists.data <- drop.missing.columns(activists.data)
parties.data <- drop.missing.columns(parties.data)
candidates.data <- drop.missing.columns(candidates.data)

####PLOTS####

# First: decide how many lines to read in, and where to start reading in. Use the same general process that was used in 
# GLOBALS and TURTLES sections above
D1.begin <- (which(temporary=="\"D1\"") - 1)/84 + 1 #which line in csv file begins the D1 section
D2.begin <- (which(temporary=="\"D2\"") - 1)/84 + 1 #which line in csv file begins the D2 section
D3.begin <- (which(temporary=="\"D3\"") - 1)/84 + 1 #which line in csv file begins the D3 section
D4.begin <- (which(temporary=="\"D4\"") - 1)/84 + 1 #which line in csv file begins the D3 section

plot.1.skip <- D1.begin + 13 #line number to start reading in data for dimension 1
plot.1.end <- D2.begin - 2 #line number to stop reading in data for dimension 1
plot.2.skip <- D2.begin + 13 #line number to start reading in data for dimension 2
plot.2.end <- D3.begin - 2 #line number to stop reading in data for dimension 2
plot.3.skip <- D3.begin + 13 #line number to start reading in data for dimension 3
plot.3.end <- D4.begin - 2 #line number to stop reading in data for dimension 3

#Next, create three vectors of column names for each dimension: D1, D2, and D3
D1.names <- scan(file="NetLogo.csv", skip=plot.1.skip - 1, nlines=1, what=" ", sep=",") #read in D1 column names
D2.names <- scan(file="NetLogo.csv", skip=plot.2.skip - 1, nlines=1, what=" ", sep=",") #read in D1 column names
D3.names <- scan(file="NetLogo.csv", skip=plot.3.skip - 1, nlines=1, what=" ", sep=",") #read in D1 column names

#Next, read in data for each dimension
lines.to.read.1 <- plot.1.end - plot.1.skip
lines.to.read.2 <- plot.2.end - plot.2.skip
lines.to.read.3 <- plot.3.end - plot.3.skip

D1.data <- scan(file="NetLogo.csv", skip=(plot.1.skip), nlines=lines.to.read.1, what=" ", sep=",")
D2.data <- scan(file="NetLogo.csv", skip=(plot.2.skip), nlines=lines.to.read.2, what=" ", sep=",")
D3.data <- scan(file="NetLogo.csv", skip=(plot.3.skip), nlines=lines.to.read.3, what=" ", sep=",")

#A quick look at the NetLogo file reveals that none of these columns have vector elements, so we don't have to 
# waste time dealing with that. Instead we can just convert each of these data objects above into dataframes,
# and then add column names and read them as .csv files. 
D1.data <- data.frame(matrix(D1.data, nrow=lines.to.read.1, byrow=TRUE))
D2.data <- data.frame(matrix(D2.data, nrow=lines.to.read.2, byrow=TRUE))
D3.data <- data.frame(matrix(D3.data, nrow=lines.to.read.3, byrow=TRUE))

#Add column names
colnames(D1.data) <- D1.names
colnames(D2.data) <- D2.names
colnames(D3.data) <- D3.names

#Clean up the dataframes by dropping columns with all missing values
drop.missing.columns2 <- function(dataset){
  for(i in ncol(dataset):1){
    if(all(dataset[,i]=="")){ #If statement tests to see if each column contains only missing values; if it does
      dataset <- dataset[,-i]       #the next line of code drops that column
    }
  }
  return(dataset)
}
D1.data <- drop.missing.columns2(D1.data)
D2.data <- drop.missing.columns2(D2.data)
D3.data <- drop.missing.columns2(D3.data)

#Next we create the WINNERS dataset which we can turn into a .csv
Winners.begin <- ((which(temporary=="\"WINNERS\"") - 1)/84 + 1) + 10 #I add 10 at the end to avoid reading 
                                                                      #in the miscellaneous information
Winners.end <- (which(temporary=="\"POLARIZATION\"") - 1)/84 - 1 #returns 9309
Winners.lines.to.read <- Winners.end - Winners.begin
Winners.colnames <- scan(file="NetLogo.csv", skip=(Winners.begin - 1), nlines=1, what=" ", sep=",")
Winners.data <- scan(file="NetLogo.csv", skip=(Winners.begin), nlines=Winners.lines.to.read, what=" ", sep=",")
Winners.data <- data.frame(matrix(Winners.data, nrow=Winners.lines.to.read, byrow=TRUE))
colnames(Winners.data) <- Winners.colnames
Winners.data <- drop.missing.columns2(Winners.data)

#Next we create the POLARIZATION .csv
polarization.begin <- ((which(temporary=="\"POLARIZATION\"") - 1)/84 + 1) + 10 #I add 10 at the end to avoid reading 
                                                                              #in the miscellaneous information
polarization.end <- (which(temporary=="\"INCUMBENT\"") - 1)/84 - 1 #returns 9490
polarization.lines.to.read <- polarization.end - polarization.begin
polarization.colnames <- scan(file="NetLogo.csv", skip=(polarization.begin - 1), nlines=1, what=" ", sep=",")
polarization.data <- scan(file="NetLogo.csv", skip=(polarization.begin), nlines=polarization.lines.to.read, what=" ", sep=",")
polarization.data <- data.frame(matrix(polarization.data, nrow=polarization.lines.to.read, byrow=TRUE))
colnames(polarization.data) <- polarization.colnames
polarization.data <- drop.missing.columns2(polarization.data)

#Finally, create the INCUMBENT .csv
incumbent.begin <- ((which(temporary=="\"INCUMBENT\"") - 1)/84 + 1) + 8 #I add 8 at the end to avoid reading 
#in the miscellaneous information
incumbent.end <- (which(temporary=="EXTENSIONS") - 1)/84 - 1 #returns 9669
incumbent.lines.to.read <- incumbent.end - incumbent.begin
incumbent.colnames <- scan(file="NetLogo.csv", skip=(incumbent.begin - 1), nlines=1, what=" ", sep=",") #vector of column names
incumbent.data <- scan(file="NetLogo.csv", skip=(incumbent.begin), nlines=incumbent.lines.to.read, what=" ", sep=",")
incumbent.data <- data.frame(matrix(incumbent.data, nrow=incumbent.lines.to.read, byrow=TRUE))
colnames(incumbent.data) <- incumbent.colnames
incumbent.data <- drop.missing.columns2(incumbent.data)

####WRITING THE FILES####
setwd("C:/Users/Thomas/Desktop") #sets the working directory to wherever you want the file stored
dir.create(main.file.name) #creates the main folder that everything else gets stored in; REMINDER: main.file.name
                            #was one of the first objects we created up above
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, sep="")) #resets the working directory to the main folder
dir.create("GLOBALS")
dir.create("TURTLES")
dir.create("PLOTS")

#Next, we use the dump() function to write the .R file for the Globals object into the GLOBALS folder. We start
#by redefining the working directory:
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/GLOBALS", sep=""))
dump("globals", file="Globals.R")

#Next, write .csv files to TURTLE subdirectory. Start by redefining the working directory, then write in each file
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/TURTLES", sep=""))
write.csv(districts.data, file="Districts.csv")
write.csv(voters.data, file="Voters.csv")
write.csv(activists.data, file="Activists.csv")
write.csv(parties.data, file="Parties.csv")
write.csv(candidates.data, file="Candidates.csv")

#Next, create subdirectories to the PLOTS directory. Begin by redefining the working directory:
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/PLOTS", sep=""))
dir.create("PositionPlot")
dir.create("WinnersPlot")
dir.create("PolarizationPlot")
dir.create("IncumbentsPlot")

#Next create the 3 .csv files for the PositionPlot folder:
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/PLOTS", "/PositionPlot", sep=""))
write.csv(D1.data, file="D1.csv")
write.csv(D2.data, file="D2.csv")
write.csv(D3.data, file="D3.csv")

#Next create the .pdf file for this folder:
pdf(file="Positions.pdf")
plot(as.numeric(as.character(D1.data$x)), as.numeric(as.character(D1.data$y)), type="l", col="red", 
     xlab="Simulation", ylab="Average Position", main="Dimension 1", ylim=c(-5, 5))
lines(as.numeric(as.character(D1.data$x.1)), as.numeric(as.character(D1.data$y.1)), type="l", col="blue")
lines(as.numeric(as.character(D1.data$x.2)), as.numeric(as.character(D1.data$y.2)), type="l", col="red", lty=2)
lines(as.numeric(as.character(D1.data$x.5)), as.numeric(as.character(D1.data$y.5)), type="l", col="blue", lty=2)
lines(as.numeric(as.character(D1.data$x.3)), as.numeric(as.character(D1.data$y.3)), type="l", col="red", lty=3)
lines(as.numeric(as.character(D1.data$x.4)), as.numeric(as.character(D1.data$y.4)), type="l", col="blue", lty=3)
legend("top", c("Rep. Candidate", "Dem. Candidate", "Rep. Voter", "Dem. Voter", "Rep. Activist", "Dem. Activist"), 
       col=c("red", "blue", "red", "blue", "red", "blue"), lty=c(1, 1, 2, 2, 3, 3), cex=0.75, ncol=2)

plot(as.numeric(as.character(D2.data$x)), as.numeric(as.character(D2.data$y)), type="l", col="red", 
     xlab="Simulation", ylab="Average Position", main="Dimension 2", ylim=c(-15, 15))
lines(as.numeric(as.character(D2.data$x.1)), as.numeric(as.character(D2.data$y.1)), type="l", col="blue")
lines(as.numeric(as.character(D2.data$x.2)), as.numeric(as.character(D2.data$y.2)), type="l", col="red", lty=2)
lines(as.numeric(as.character(D2.data$x.5)), as.numeric(as.character(D2.data$y.5)), type="l", col="blue", lty=2)
lines(as.numeric(as.character(D2.data$x.3)), as.numeric(as.character(D2.data$y.3)), type="l", col="red", lty=3)
lines(as.numeric(as.character(D2.data$x.4)), as.numeric(as.character(D2.data$y.4)), type="l", col="blue", lty=3)
legend("top", c("Rep. Candidate", "Dem. Candidate", "Rep. Voter", "Dem. Voter", "Rep. Activist", "Dem. Activist"), 
       col=c("red", "blue", "red", "blue", "red", "blue"), lty=c(1, 1, 2, 2, 3, 3), cex=0.75, ncol=2)

plot(as.numeric(as.character(D3.data$x)), as.numeric(as.character(D3.data$y)), type="l", col="red", 
     xlab="Simulation", ylab="Average Position", main="Dimension 3", ylim=c(-4, 4))
lines(as.numeric(as.character(D3.data$x.1)), as.numeric(as.character(D3.data$y.1)), type="l", col="blue")
lines(as.numeric(as.character(D3.data$x.2)), as.numeric(as.character(D3.data$y.2)), type="l", col="red", lty=2)
lines(as.numeric(as.character(D3.data$x.5)), as.numeric(as.character(D3.data$y.5)), type="l", col="blue", lty=2)
lines(as.numeric(as.character(D3.data$x.3)), as.numeric(as.character(D3.data$y.3)), type="l", col="red", lty=3)
lines(as.numeric(as.character(D3.data$x.4)), as.numeric(as.character(D3.data$y.4)), type="l", col="blue", lty=3)
legend("top", c("Rep. Candidate", "Dem. Candidate", "Rep. Voter", "Dem. Voter", "Rep. Activist", "Dem. Activist"), 
       col=c("red", "blue", "red", "blue", "red", "blue"), lty=c(1, 1, 2, 2, 3, 3), cex=0.75, ncol=2)
dev.off()

#Next create the .csv file for the WinnersPlot folder:
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/PLOTS", "/WinnersPlot", sep=""))
write.csv(Winners.data, file="Winner.csv")

#Next we create the .pdf file:
pdf(file="Winner.pdf")
par(mfrow=c(1,1))
plot(as.numeric(as.character(Winners.data[,1])), as.numeric(as.character(Winners.data[,2])), type="l", col="blue", 
     xlab="Time Period", ylab="Percent of Vote")
lines(as.numeric(as.character(Winners.data[,5])), as.numeric(as.character(Winners.data[,6])), type="l", col="black")
lines(as.numeric(as.character(Winners.data[,9])), as.numeric(as.character(Winners.data[,10])), type="l", col="red")
dev.off()

#Next create the .csv file for the Polarization directory:
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/PLOTS", "/PolarizationPlot", sep=""))
write.csv(polarization.data, file="Polarization.csv")

#Next we create the .pdf file:
pdf(file="Polarization.pdf")
plot(as.numeric(as.character(polarization.data[,1])), as.numeric(as.character(polarization.data[,2])), type="l", col="black", 
     xlab="Pair of Candidates/Activists/Voters", ylab="Degree of Partisanship", ylim=c(0, 10))
lines(as.numeric(as.character(polarization.data[,5])), as.numeric(as.character(polarization.data[,6])), type="l", col="dark green")
lines(as.numeric(as.character(polarization.data[,9])), as.numeric(as.character(polarization.data[,10])), type="l", col="purple")
legend("bottomright", c("Candidates", "Voters", "Activists"), col=c("black", "dark green", "purple"), lty=c(1, 1, 1), cex=0.75)
dev.off()

#Next create the .csv file for the Incumbents directory:
setwd(paste("C:/Users/Thomas/Desktop", "/", main.file.name, "/PLOTS", "/IncumbentsPlot", sep=""))
write.csv(incumbent.data, file="IncumbentWins.csv")

#Finally, we create the .pdf file for the Incumbents subdirectory:
pdf(file="IncumbentWins.pdf")
plot(as.numeric(as.character(incumbent.data[,1])), as.numeric(as.character(incumbent.data[,2])), type="l", col="black", 
     xlab="Time Period", ylab="Percent of Incumbents in Each Party Who Win Election")
dev.off()




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
