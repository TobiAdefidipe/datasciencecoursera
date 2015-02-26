rankall <- function (outcome, num ="best") {  
  
  data_raw <- read.csv ("outcome-of-care-measures.csv", header = T, colClasses = "character")  #Read Outcome data
  data <- (data_raw[, c(2,7,11,17,23)])
  names (data) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  hospital_list <- data.frame()
  nums <- data.frame()
  #Invalid outcome condition
  if (!outcome%in%colnames(data[3:5])){
    stop ("invalid outcome")
  }
  #Set conditions for num 
  rord = "FALSE" #default
  
  if(num == "best"){
    num <- 1}
  if(num == "worst"){
    num <- 1
    rord <- "TRUE"
  } 
  
  states <- unique(data$State,na.omit=T)
  
  for (i in 1:length(states)){
    
    state_list <- data[data$State==states[i],] #select data for specific state
    state_list[,outcome] <- suppressWarnings (as.numeric (state_list[, outcome]))
    state_list <- state_list[with(state_list,order(state_list[[outcome]],state_list[1], decreasing = rord)),]#Order according to rank in outcomes & alphabetical
    #View(state_list)
    state_list <- na.omit(state_list)
    state_list <- data.frame (state_list$Hospital, state_list$State)
    colnames(state_list) <- c("hospital", "state")
        
    #chek if num is valid (i.e if data exist for that state)
    
    state_list_NA <- data.frame ("<NA>", states[i] ) #default output
    colnames(state_list_NA) <- c("hospital", "state")
    
    if(num <= nrow(state_list)) {
      hospital <- state_list [num,] #pick the numth column of data
    } else {
      hospital <- state_list_NA
    }
    
    hospital_list <- rbind(hospital_list, hospital)
    colnames(hospital_list) <- c("hospital", "state")
  }     
  output <- hospital_list[with(hospital_list,order(hospital_list[2])),]
  print (output)       
}   

