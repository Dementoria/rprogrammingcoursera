pollutantmean <- function(directory, pollutant, id = 1:332) {
    # initiates an empty vector for storing the data from individual files
    poldata <- vector()
    # iterates over the individual ids in the vector id
    for(i in id) {
        # if it is less than 10, it needs to begin with "00"
        if(i < 10) {
            fn <- paste("00", i, sep = "")
        # if it is less than 100, it needs to begin with "0"
        } else if(i < 100) {
            fn <- paste("0", i, sep = "")
        } else {
        # otherwise, just take the id and make it a character
            fn <- as.character(i)
        }
    # create the path as a string and store it in a variable
    file <- paste(directory, "/", fn, ".csv", sep = "")
    # get the data from the file and store it
    data <- read.csv(file)
    # subset the data based on the pollutant and combine it with the empty vector created on line 2
    poldata <- c(poldata, data[[pollutant]])
    }
    # calculate the mean of all the values, ignoring NAs and return it
    mean(poldata, na.rm = TRUE)
}