rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data.dir <- "./DataScience/ProgrammingAssignment3/data"
    data <- read.csv(paste(data.dir, "outcome-of-care-measures.csv", sep = "/"), colClasses = "character")
    
    ## Check that outcome is valid
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
    
    
    ## Select data where outcome is filled
    data <- data[ data[col] != "Not Available", c("State", "Hospital.Name", col)]
    
    ## Converts outcom from character to numeric
    data[, 3] <- as.numeric(data[, 3])
    
    ## Sort by outcome and hospital name
    data <- data[ order(data[,3], data$"Hospital.Name"), ]
    
    ## Splits data by state
    data <- split(data, data$"State")
    
    ## Select hospitals based on position in outcome ranking from each state
    result <- data.frame()
    for ( i in 1:length(data) ) {
        df <- data.frame(data[i])
        
        if ( num == -1) { selected <- df[length(df[, 2]), 2:1]
        } else if ( num > length(df[, 2])) { selected <- data.frame( NA , df[1,1])
        } else selected <- df[num, 2:1]
        
        names(selected) <- c("hospital", "state")
        result <- rbind(result, selected)
    }
    
    row.names(result) <- result[,2]
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    result
}