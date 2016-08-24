complete <- function(directory, id = 1:332) {
    csvFiles <- list.files(path = directory, full.name = TRUE, pattern = "*.csv")[id]
    data <- lapply(csvFiles, read.csv)
    
    nobs <- c()
    for(i in 1:length(id)) {
        sulfateFilled <- !is.na(data[[i]][['sulfate']])
        nitrateFilled <- !is.na(data[[i]][['nitrate']])
        completeDataRows <- sum(sulfateFilled & nitrateFilled)
        nobs <- c(nobs, completeDataRows)
    }
    data.frame(id,nobs)
}
