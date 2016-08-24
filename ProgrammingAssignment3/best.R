best <- function(state, outcome) {
    ## Reads outcome data
    data.dir <- "./DataScience/ProgrammingAssignment3/data"
    data <- read.csv(paste(data.dir, "outcome-of-care-measures.csv", sep = "/"), colClasses = "character")
    
    ## Check that state and outcome are valid
    if ( !(state %in% data$"State") ) stop("invalid state")
    if ( !(outcome %in% c("heart attack", "heart failure", "pneumonia")) ) stop("invalid outcome")
    
    ## Select column for analysis based on outcome
    if (outcome == "heart attack") {
        col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else {
        col <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    ## Select data for given state and where outcome is filled
    data <- data[data["State"] == state & data[col] != "Not Available", c("Hospital.Name", col)]
    
    ## Converts outcom from character to numeric
    data[, 2] <- as.numeric(data[, 2])
    
    ## Sort by outcome and hospital name
    data <- data[ order(data[,2], data[,1]), ]
    
    ## Select best hospital based on outcome
    best <- data[1, 1]
    
    ## Return hospital name in that state with lowest 30-day death rate
    best
}
