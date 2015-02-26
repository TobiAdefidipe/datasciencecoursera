complete <- function (directory, id = 1:332) {
  
  files_list <- list.files(directory, full.names=TRUE)  
  output <- data.frame()
  
  for (i in id) { 
    
    # reads each file specified 
    dat <-  read.csv(files_list[i])
    
    subset_nitrate <- subset(dat, !is.na(dat$nitrate))
    subset_nitrate_sulfate <- subset (subset_nitrate, !is.na(subset_nitrate$sulfate))
    
    n_obj <- nrow (subset_nitrate_sulfate)
    out <- c(i, n_obj)
    
    output <- rbind (output, out)
    names <- c("id", "nobs")
    colnames (output) <- names
}
output

}
