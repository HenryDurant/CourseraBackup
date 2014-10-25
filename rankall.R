rankall <- function(outcome, num = "best"){
        ##Read outcome data
        
outcome1 <- read.csv("outcome-of-care-measures.csv")
        
        ##Check that state and outcome are valid
        
##Subset out unwanted columns
outcome2 <- outcome1[ ,c(2, 7, 11, 17, 23)] 

outcome3 <- sapply(outcome2,function(x) gsub("Not Available", "NA", x))
outcome4 <- as.data.frame(outcome3)

colnames(outcome4) <- c("Hospital_Name", "State", "heart attack", 
                        "heart failure", "pneumonia") 

##if statement to stop if no match for outcome
options <- c("heart attack", "heart failure", "pneumonia")
x <- grep(outcome, options)
if (x == 0){
        stop("invalid outcome")
}
        
        ##For each state, find the hospital of the given rank
        
data1 <- outcome4[ ,outcome]
Outcome <- suppressWarnings(as.numeric(as.character(data1)))
Hospital1 <- outcome4[ ,1]
hospital <- as.character(Hospital1)
state1 <- outcome4[ ,2]
state <- as.character(state1)
data2 <- data.frame(Outcome, hospital, state)
data3 <- data2[complete.cases(data2), ]

data4 <- split(data3, data3$state )


##order according to outcome/state alphabetical order
data5 <- lapply(data4, function(x) x[order(x[ ,1], x[ ,2]), ])

##Create column "rank" showing 

Rank <- lapply(data5, function(x) 1:(nrow(x)))

##Change first and last values in each data frame of Rank
as.character(Rank)
Rank1 <- lapply(function(x) lapply(x, function(x) replace(x, 1, "best")))

str(Rank1)

Rank1 <- lapply(Rank, function(x) Rank[x]="best")

Rank[1] = "best"
Rank[nrow(data5)] = "worst"


##Stick data6 to Rank
data6 <- vapply(data5, cbind, Rank)

##Subset the rank wanted (num)
data7 <- subset(data6, Rank == num)
data8 <- (data7[ ,3])
data9 <- as.character(data8)
print(data9)



        ##Return a data frame with the hospital names and the
        ##abbreviated state name
        
}