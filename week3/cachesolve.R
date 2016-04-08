cacheSolve <- function (x, ...) {
    # sets i to the result of calling getinverse on variable x
    i <- x$getinverse()
    # checks whether i = NULL, that is, has it been set using x$setinverse?
    if(!is.null(i)) {
        # if it has been set, return a message and variable i
        message("getting cached data")
        return(i)
    }
    # if it hasn't been set, use the x$get function to get the matrix and
    # store it in the variable 'data'
    data <- x$get()
    # set i to the inverse (using solve) of the variable 'data'
    i <- solve(data, ...)
    # call x$setinverse to set the i variable to the inverse of the matrix
    x$setinverse(i)
    # return i 
    i
}