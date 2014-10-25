
pollutantmean <- function(directory, pollutant, id=1:332){

  #Set Directory - character vector length 1 

con <- file.path(getwd(), directory)
  
  #Set id vector - integer vector length 1

id2 <- formatC(id, width = 3, flag = "0")

  #Get Filelist
filelist <- paste(con, "/", id2, ".csv", sep="")   

  #Import Data
data = do.call("rbind", lapply(filelist, function(x) read.csv(x,header = TRUE)))

  #Get mean of set pollutant
   
m = mean(data[ ,pollutant], na.rm = TRUE)
round(m, digits = 3)
}



