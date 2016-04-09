rankhospital <- function(state, outcome, num = "best") {
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

    # arranges the 'statedata' by outcome value and then hospital
    # name, as according to the instructions
    sortedstate <- statedata[order(statedata[,2], statedata[,1]),]

    # binds a column with the rank to the 'sortedstate' data
    sortedstate <- cbind(sortedstate, rank = 1:nrow(sortedstate))

    # removes any NAs by using only complete cases
    completesortedstate <- sortedstate[complete.cases(sortedstate),]

    # if statement checks if the character vector == "best" or "worst" was passed in
    # also checks to see whether or not the numeric passed in is greater than
    # number of rows in the sorted data
    if(num == "best") {
        num <- 1
    } else if(num == "worst") {
        num <- nrow(completesortedstate)
    } else if(as.numeric(num) > nrow(completesortedstate)) {
        return(NA)
    } else {
        num <- as.numeric(num)
    }

    # selects a specific row based on it's rank value and 
    # returns only the first column, the hospital's name
    selected <- completesortedstate[completesortedstate$rank == num, 1]
    
    # returns the selected hospital
    selected
}