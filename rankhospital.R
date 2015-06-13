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

rankhospital <- function(state, outcome, num = "best") {
  
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
  stateCheck <- unique(dataRaw$State) #valid list of states - need to read the file first
  
  if(!any(stateCheck == state)) stop("invalid state")
  else if(!any(outcomeCheck == outcome)) stop("invalid outcome")
  
  
  #grab only the rows of the selected state and only the 5 required columns
  dataRaw <- dataRaw[dataRaw$State==state, c(2, 7, 11, 17, 23)]
  #overwrite column names to facilitate subsetting 
  names(dataRaw) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  
  #order by outcome to get the best rank and by name to resolve the ties 
  dataRaw <- dataRaw[order(dataRaw[[outcome]],dataRaw$name),] 
  
  #return the name of hospiatal based on num
  if (is.numeric(num)) ranking <- num
  else if(num == "best") ranking <- 1
  else if(num == "worst") ranking <- sum(!is.na(dataRaw[[outcome]]))  #returns the last non-NA element 
  dataRaw[ranking,"name"]
}