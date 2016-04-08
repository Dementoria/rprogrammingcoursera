makeCacheMatrix <- function(x = matrix()) {
    # set the inverse (i) to be NULL, initially
    i = NULL
    # make a set function
    set <- function(y) {
        # set the external x to be the value passed to set function as y
        x <<- y
        # make the external variable i equal to NULL when setting a new x
        i <<- NULL
    }
    # make a get  function that returns x
    get <- function() { x }
    # make a setinverse function that sets the inverse of x
    setinverse <- function(solve) i <<- solve
    # make a getinverse function that return the inverse of x
    getinverse <- function() { i }
    # create a list to return all of the functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}