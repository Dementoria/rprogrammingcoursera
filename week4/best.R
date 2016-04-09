best <- function(state, outcome) {
    # read in the data, set everything that says "Not Available" to be NA values
    # remove the treatment of strings as factors to avoid conversion later
    data <- read.csv("outcome-of-care-measures.csv", 
                     na.strings = "Not Available", 
                     stringsAsFactors = FALSE)

    # create a vector of outcomes to reduce space in the if statement
    outcomes <- c("heart attack", "heart failure", "pneumonia")

    # if statement checks whether or not the state passed in with the function
    # is in the list of all distinct states in the dataset
    if(!state %in% unique(data[,7])) {
        # if it isn't, stop execution and pass a message to the user
        stop("invalid state")
    }
    
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

    # creates a 'statedata' data.frame by subsetting 'data' using
    # the state name and the col variable from the if statement
    statedata <- data[data$State == state, c(2, col)]

    # finds the minimum value in the second column of statedata and sets 
    # a minval variable equal to that
    minval <- min(statedata[,2], na.rm = TRUE)

    # finds all the hospital names where the outcome value is equal to the minimum value
    names <- statedata[which(statedata[,2] == minval), 1]

    # sorts the names and stores them in alphabetical order
    sorted <- sort(names)

    # sets the final return 'name' variable equal to the first entry in the ordered
    # names vector
    name <- sorted[1]
    
    # returns the 'name' variable
    name
}