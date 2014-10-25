rankhospital <- function(state, outcome, num = "best"){
        ##Read outcome Data
        
        outcome1 <- read.csv("outcome-of-care-measures.csv")
        
        ##Subset out unwanted columns
        outcome2 <- outcome1[ ,c(2, 7, 11, 17, 23)] 
        
        outcome3 <- sapply(outcome2,function(x) gsub("Not Available", "NA", x))
        outcome4 <- as.data.frame(outcome3)
        
        
        ##Subset state 
        outcome5 <- subset(outcome4, State == state)
        
        ##Rename Columns with outcome names
        colnames(outcome5) <- c("Hospital_Name", "State", "heart attack", 
                                "heart failure", "pneumonia") 

        ##if statement to stop if no match for outcome
        options <- c("heart attack", "heart failure", "pneumonia")
        x <- grep(outcome, options)
        if (x == 0){
                stop("invalid outcome")
        }
        
        
        ##if statement to stop if no match for state 
        if (nrow(outcome5) == 0){
                stop("invalid state")
        }
        
      
data1 <- outcome5[ ,outcome]
Outcome <- suppressWarnings(as.numeric(as.character(data1)))
Hospital1 <- outcome5[ ,1]
Hospital <- as.character(Hospital1)
data4 <- data.frame(Outcome, Hospital)
data41 <- data4[order(data4[ ,1], data4[ ,2]), ]
data5 <- data41[complete.cases(data41), ]

## If statement to return error if num > number of hospitals

        if(num == "best"){
                num1 <- 1
        }
        else if(num == "worst"){
                num1 <- nrow(data5)
        }
        else num1 <- as.integer(num)


x <- nrow(data5)


if (num1 > x){
        print("NA")
        opt <- options(show.error.messages=FALSE)
        on.exit(options(opt))
        stop()
}

Rank <- 1:(nrow(data5))
Rank[1] = "best"
Rank[nrow(data5)] = "worst"
data6 <- cbind(Rank, data5)
data7 <- subset(data6, Rank == num)
data8 <- (data7[ ,3])
data9 <- as.character(data8)
print(data9)
}
