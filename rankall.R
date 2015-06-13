#Rprogrammning cousre - programming assignment 3
#Hospital Quality task 3  

#in:state: the 2-character abbreviated name of a state 
#in:outcome: outcome name(E.G. “heart attack”, “heart failure”). 
#in:num ("best","worst", int): ranking of a hospital in that state for that outcome (num)
#out: returns a character vector with the name of the hospital that has the ranking specified by the num argument
###Explude Hospitals that do not have data on a particular outcome 
###Ties in outcome should be broken by using the hospital name.
###Return NA if num > number of hospitals in that state


setwd("~/Coursera/lectures/Rprogramming/pr3")

rankall <- function(outcome, num = "best") {
  
  ##Check if file exists in the current wd
  fileName <- "outcome-of-care-measures.csv"
  if(!file.exists(fileName)){
    message("Problem with the file")
    return()
  }
  
  #read raw data define na strings, don't change strings to factors 
  dataRaw <- read.csv(fileName ,na.strings = "Not Available", stringsAsFactors = FALSE)         
  
  #validate state and outcome are valid
  outcomeCheck <-c("heart attack", "heart failure", "pneumonia")    
  if(!any(outcomeCheck == outcome)) stop("invalid outome")
  
  
  #grab only the rows of the selected state and only the 5 required columns
  names(dataRaw)[c(2, 7, 11, 17, 23)] <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  dataRaw <- dataRaw[, c("name","state",outcome)]
  
  #sort by name (state might not be necessary) to get the ties right
  #dataRaw <- dataRaw[order(dataRaw$state, dataRaw$name),] 
  dataRaw <- dataRaw[!is.na(dataRaw[[outcome]]), ]
  dataRaw <- dataRaw[order(dataRaw[[outcome]], dataRaw$name),] 
  
  dataRaw.list <- split(dataRaw,f=dataRaw$state)
  hospital.names.splitted <- lapply(dataRaw.list,function(x,n) {
  index <- ifelse(n=="worst",nrow(x),
                    ifelse(n=="best",1,n))
    x[index,1:2]
  },num)
  
  hospital.names <- do.call(rbind,hospital.names.splitted)
  colnames(hospital.names) <- c("hospital", "state")
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(hospital.names)
  
  #nums <- tapply(dataRaw[[outcome]],dataRaw$state,order)
  
}