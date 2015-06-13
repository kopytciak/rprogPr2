rankall1 <- function(outcome, num = "best") {
  ## Read outcome data
  
  ## Read outcome data
  outcome.data <- read.csv(file="outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  outcome.column = ifelse(outcome=="heart attack",11,
                          ifelse(outcome=="heart failure",17,
                                 ifelse(outcome=="pneumonia",23,NA)))
  
  ## Check that outcome is valid
  if (is.na(outcome.column))
    stop(
      paste("Error in best(",state,", ",outcome,") : invalid outcome",sep="")
    )
  
  ## For each state, find the hospital of the given rank
  outcome.data <- subset(outcome.data, select=c(2,7,outcome.column))
  # outcome.data columns:
  # 1 - Hospital Name
  # 2 - State
  # 3 - Hospital 30-Day Death Mortality Rates from requested outcome
  names(outcome.data)[3] <- outcome
  outcome.data[,3] <- suppressWarnings(as.numeric(outcome.data[,3]))
  outcome.data <- outcome.data[!is.na(outcome.data[,3]),]
  outcome.data <- outcome.data[order(outcome.data[,1]),]
  outcome.data <- outcome.data[order(outcome.data[,3]),]
  
  
  
  #outcome.data.list <- split(outcome.data,f=outcome.data$State)
  #hospital.names.splitted <- lapply(outcome.data.list,function(x,n) {
  #  index <- ifelse(n=="worst",nrow(x),
  #                  ifelse(n=="best",1,n))
  #  x[index,1:2]
  #},num)
  
  #hospital.names <- do.call(rbind,hospital.names.splitted)
  #colnames(hospital.names) <- c("hospital", "state")
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  #return(hospital.names)
}