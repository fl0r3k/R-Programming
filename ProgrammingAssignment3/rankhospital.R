rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data.dir <- "./DataScience/ProgrammingAssignment3/data"
    data <- read.csv(paste(data.dir, "outcome-of-care-measures.csv", sep = "/"), colClasses = "character")
    
    ## Check that state and outcome are valid
    if ( !(state %in% data$"State") ) stop("invalid state")
    if ( !(outcome %in% c("heart attack", "heart failure", "pneumonia")) ) stop("invalid outcome")
    
    ## Check if num has valid value
    if ( is.character(num) ) {
        if ( num == "best" ) { num <- 1
        } else if ( num == "worst" ) { num <- -1
        } else stop("invalid num")
    } else if ( is.integer(num) ) {
        if ( num <= 0 ) stop("invalid num")
    }
    
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
    
    ## Select hospital based on position in outcome ranking
    if ( num == -1) { selected <- data[length(data[, 1]), 1]
    } else selected <- data[num, 1]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    selected
}