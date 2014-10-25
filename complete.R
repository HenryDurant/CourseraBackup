
complete <- function(directory, id=1:332){
        
        #Set Directory - character vector length 1 
        
        con <- file.path(getwd(), directory)
        
        #Set id vector - integer vector length 1
        
        id2 <- formatC(id, width = 3, flag = "0")
        
        #Get Filelist
        filelist <- paste(con, "/", id2, ".csv", sep="")      
        
        #Open CSVs into list of data frames
        data <- lapply(filelist, read.csv)
        
        #Complete cases for each data frame
        comp <- lapply(data, function(x) complete.cases(x))
        
        #Sum of complete cases for each data frame
        nobs1 <- lapply(comp, function(x) sum(x))
    nobs <- unlist(nobs1)
 
        #put results in table next to IDs
        
        data.frame(id, nobs)
}