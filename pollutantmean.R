

pollutantmean <- function(directory, pollutant, id = 1:332)  {
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  dat <- data.frame()                             #creates an empty data frame
  for (i in id) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  
  if (pollutant =="nitrate") {
     mean (dat$nitrate, na.rm=TRUE)
}
else{
    mean (dat$sulfate, na.rm=TRUE)
}

}