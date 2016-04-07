corr <- function(directory, threshold = 0) {
    # read all files in to a list
    allfiles <- list.files(directory, full.names = TRUE)
    
    # initialise the vector
    corr <- vector()
    
    # iterate over the list
    for (i in allfiles) {
        # read the data in
        data <- read.csv(i)
        if (sum(complete.cases(data)) > threshold) {
            #subset based on complete cases
            comp <- data[complete.cases(data),]
            #calculate the correlation between the pollutants
            corr <- c(corr, cor(comp$nitrate, comp$sulfate))
        }
    }
    # return the vector containing the correlations
    corr
}