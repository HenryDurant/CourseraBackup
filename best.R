
best <- function(state, outcome){

        ##Read outcome data
outcome1 <- read.csv("outcome-of-care-measures.csv")

##Subset out unwanted columns
outcome2 <- outcome1[ ,c(2, 7, 11, 17, 23)] 

outcome3 <- sapply(outcome2,function(x) gsub("Not Available", "NA", x))
outcome4 <- as.data.frame(outcome3)

##Make Columns 3 to 5 numeric
outcome4[, 3:5] <- sapply(outcome4[, 3:5], as.numeric)

##if statement to stop if no match for outcome
options <- c("heart attack", "heart failure", "pneumonia")
x <- grep(outcome, options)
if (x == 0){
        stop("invalid outcome")
}

##Subset state 
outcome5 <- subset(outcome4, State == state)

##if statement to stop if no match for state 
if (nrow(outcome5) == 0){
        stop("invalid state")
}

        ##Rename Columns with outcome names
colnames(outcome5) <- c("Hospital_Name", "State", "heart attack", 
                        "heart failure", "pneumonia") 

        ##Retrieve Mimnimum

outcome6 = outcome5[which(outcome5[,outcome]==min(outcome5[outcome])), 1]

outcome7 <- sort(outcome6)

print(outcome7[1])

}




