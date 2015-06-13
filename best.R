#Rprogrammning cousre - programming assignment 3
#Hospital Quality task 2  

#in: the 2-character abbreviated name of a state and an outcome name(E.G. “heart attack”, “heart failure”). 
#out: character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#     in that state.  Hospitals that do not have data on a particular outcome 
#     should be excluded from the set of hospitals when deciding the rankings.

setwd("~/Coursera/lectures/Rprogramming/pr3")
  
best <- function(state, outcome) {
  
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
  else if(!any(outcomeCheck == outcome)) stop("invalid outome")
   
  #grab only the rows of the selected state and only the 5 required columns
  dataRaw <- dataRaw[dataRaw$State==state, c(2, 7, 11, 17, 23)]
  #overwrite column names to facilitate subsetting 
  names(dataRaw) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  
  #order by outcome to get the best rank and by name to resolve the ties 
  dataRaw <- dataRaw[order(dataRaw[[outcome]],dataRaw$name),] 
  #return the name of the first item in list ordered as above 
  dataRaw[1,"name"]
}