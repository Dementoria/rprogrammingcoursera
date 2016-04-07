complete <- function(directory, id = 1:322) {
    # initialise an empty data frame for storing the ids and number of complete cases
    df <- data.frame()
    # iterate over the ids passed in the the id vector
    for(i in id) {
        # if it is less than 10, it needs to begin with "00"
        if(i < 10) {
            fn <- paste("00", i, sep = "")
        # if it is less than 100, it needs to begin with "0"
        } else if(i < 100) {
            fn <- paste("0", i, sep = "")
        # otherwise, just take the id and make it a character
        } else {
            fn <- as.character(i)
        }
        # create the path as a string and store it in a variable
        file <- paste(directory, "/", fn, ".csv", sep = "")
        # get the data from the file and store it
        data <- read.csv(file)
        # count the number of complete cases and store it with the id as a new row to be bound to df
        newrow <- c(i, sum(complete.cases(data)))
        # bind the new row with the data frame
        df <- rbind(df, newrow)
    }
    # name the columns
    colnames(df) <- c("id", "nobs")
    # return the data frame
    df
}