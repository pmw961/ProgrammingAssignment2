##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## These two functions operate in tandem to calculate the inverse of a square
## matrix. As this is a time intensive operation, the functions check to see
## if the calculations has been performed previously and, if so, returns the
## result from memory.

## To run this from the console, begin with a square matrix M
## Call the makeCacheMatrix as shown below
##
##   N <- makeCacheMatrix(M)
##
## The result can then be passed to the cacheSolve function to calculate the
## inverse matrix
##
##   cacheSolve(N)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## The makeCacheMatrix function takes a square matrix 'x' and returns a list of
## attributes and functions to be sent to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse values to null
    i <- NULL
    # Define the set function. These variables are accessible outside the local
    # environment
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Define the get function which returns the variable 'x'
    get <- function() x
    # Define the setinv function which sets the variable 'i' to be accessible
    # outside the local environment
    setinv <- function(inv) i <<- inv
    # Define the getinv function to return the matrix inverse if it has been
    # previously calculated
    getinv <- function() i
    # The makeCacheMatrix returns a list of these defined functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## The cacheSolve function solves for the inverse of a square matrix. It first
## checks to see if the inverse has previously been calculated. It requires
## the output of the makeCacheMatrix function as its input

cacheSolve <- function(x, ...) {
    # Check if the inverse has previously been calculated for this matrix
    i <- x$getinv()
    # If the inverse has been calculated, return the result
    if(!is.null(i)) {
        message("getting cached data")
        # No further calculation required, return the result and exit the
        # function
        return(i)
    }
    # Load the matrix into the data variable
    data <- x$get()
    # Solve for the inverse of data
    i <- solve(data, ...)
    # Store the inverse into the list object to keep the result cached for
    # future use if necessary
    x$setinv(i)
    # Return the inverse result
    i    
}
