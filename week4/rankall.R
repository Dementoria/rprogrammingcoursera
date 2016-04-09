rankall <- function(outcome, num = "best") {
    # read in the data, set everything that says "Not Available" to be NA values
    # remove the treatment of strings as factors to avoid conversion later
    data <- read.csv("outcome-of-care-measures.csv", 
                     na.strings = "Not Available",
                     stringsAsFactors = FALSE)
    
    # create a vector of all the states that appear in the dataset
    allstates <- unique(data[,7])
    
    # create a vector of outcomes to reduce space in the if statement
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    # if statement checks whether or not the outcome passed in with the function
    # is in the outcomes vector
    if(!tolower(outcome) %in% outcomes) {
        # if it isn't, stop execution and pass a message to the user
        stop("invalid outcome")
    } else if(tolower(outcome) == outcomes[1]) {
        # if the outcome is equal to "heart attack", use column number 11
        # I've used to lower to allow the program to keep working when input
        # outcome is capitalised 
        col <- 11
    } else if(tolower(outcome) == outcomes[2]) {
        # if the outcome is equal to "heart failure", use column number 17
        col <- 17
    } else if(tolower(outcome) == outcomes[3]) {
        # if the outcome is equal to "pneumonia", use column number 23
        col <- 23
    }
    
    # create an empty data.frame with two columns, also setting strings to
    # not be registered as factors when being added
    results <- data.frame(state = character(), hospital = character(), stringsAsFactors = FALSE)
    
    
    # set an initial variable x equal to 1
    x <- 1
    
    # loop through each state in the 'allstates' vector created before
    for(i in allstates) {
        # for each state (i) call the rankhospital function with the
        # parameters passed in to the rankall function call
        # and store the result as a new row in the data frame
        results[x,] <- c(i, rankhospital(i, outcome, num))
        # increment x
        x <- x + 1
    }
    
    # order the results by state abbreviation
    orderedresults <- results[order(results[,1]),]
    
    # return the ordered results
    orderedresults
}