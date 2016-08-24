corr <- function(directory, threshold = 0) {
    csvFiles <- list.files(path = directory, full.name = TRUE, pattern = "*.csv")
    data <- lapply(csvFiles, read.csv)
    
    cr <- c()
    for(i in 1:length(data)) {
        dateFilled <- !is.na(data[[i]][['Date']])
        idFilled <- !is.na(data[[i]][['ID']])
        sulfateFilled <- !is.na(data[[i]][['sulfate']])
        nitrateFilled <- !is.na(data[[i]][['nitrate']])
        rows <- dateFilled & idFilled & sulfateFilled & nitrateFilled
        completeDataRows <- sum(rows)
        if (completeDataRows > threshold) {
            sulfateVector <- data[[i]][['sulfate']][rows]
            nitrateVector <- data[[i]][['nitrate']][rows]
            cr <- c(cr, cor(nitrateVector, sulfateVector))
        }
    }
    cr
}
