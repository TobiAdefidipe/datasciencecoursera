rankhospital <- function (state, outcome, num ="best") { # state is a 2 character abbreviation, # outcome is the file  
  
  data <- read.csv ("outcome-of-care-measures.csv", colClasses = "character")  #Read Outcome data
  
  state_names <- data$State
  outcome_names <- c ("heart attack", "heart failure", "pneumonia")
  
  if (outcome == "heart attack") {out_col <- 11} 
  if (outcome == "heart failure") {out_col <- 17} 
  if (outcome == "pneumonia") {out_col <- 23} 

  
  if (any(state_names == state)) { #check if state is valid
    if (any(outcome_names == outcome)) {  ##check if outcome is valid 
      
      subset_state <- subset (data, data$State == state) #Get only data correspinding to that state
      subset_state[,out_col] <- suppressWarnings (as.numeric (subset_state[,out_col])) #convert ranking data to numeric & supress warnings
     # View (subset_state)
      sorted_data <- subset_state[order(subset_state[,out_col],subset_state[,2]),] #sort data accroding to rank and then alphabetically
      sorted_data <- na.omit(sorted_data)

      
       if (num =="'best") {
         num <- 1
       }else { if (num == "worst") { 
           num <- nrow (sorted_data) 
         }else {if (num > nrow(sorted_data)){
             hospital <- NA
           }
         }
       }

      hospital <- sorted_data [num,2] #getdata in first row i.e higest ranking hospital
      
    } else
      stop ('invalid outcome')
    
  } else
    stop ('invalid state')
  
  print (hospital)
}
