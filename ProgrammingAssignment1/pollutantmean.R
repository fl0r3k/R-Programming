pollutantmean <- function(directory, pollutant, id=1:332) {
    csvFiles <- list.files(path = directory, full.name = TRUE, pattern = "*.csv")[id]
    data <- lapply(csvFiles, read.csv)
    
    pollutantVector <- c()
    for(i in 1:length(id)) {
        pollutantVector <- c(pollutantVector, data[[i]][[pollutant]])
    }
    
    mean(pollutantVector, na.rm = TRUE)
}
