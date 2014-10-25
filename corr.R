##METHOD USING SEPPERATE DATA FRAMES
corr <- function(directory, threshold = 0){
        
        #Set Directory - character vector length 1 
con <- file.path(getwd(), directory)
        
        #Get Filelist
filelist1 <- list.files(path = con, pattern = ".csv")
filelist <- paste(con, "/", filelist1, sep = "")
        
        #Open CSVs into list of data frames
data <- lapply(filelist, read.csv)
        
        #Complete cases for each data frame
comp <- lapply(data, function(x) complete.cases(x))
        
        #Sum of complete cases for each data frame
nobs <- sapply(comp, function(x) sum(x))
        
        #Create a vector of integers corresponding to number of items in nobs
id = 1:length(nobs)
        
        #Table of complete cases with ids attached
table = cbind(id, nobs)
        #delete rows with less than threshold
table2 <- subset(table, nobs > threshold)  
        
        #extract row with new id numbers
idnew1 <- table2[ ,1]


        #if statement to stop function if nothing meets criteria
if (length(idnew1) == 0){
        return(idnew1)
}
       else{
                
        #Get New Filelist
idnew <- formatC(idnew1, width = 3, flag = "0")
filelist2 <- paste(con, "/", idnew, ".csv", sep="")      
          
          #Open CSVs into list of data frames
data2 <- lapply(filelist2, read.csv) 
          
          #Get correlations
          
correlations <- sapply(data2, function(x) cor(x$sulfate, x$nitrate, use = "complete", method="pearson"))
          
        }
}
        
       
        
     

