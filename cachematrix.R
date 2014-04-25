## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## set up local inverse variable that (using R's cooky scoping rules) will hold
  ## the inverse when it is set
        inverse <- NULL
        ## create a set function in case we need to change the matrix in the list object
        ## we create
        set <- function(changeToThisMatrix) {
                if (is.matrix(changeToThisMatrix))
                {
                        x <<- changeToThisMatrix
                        inverse <- NULL
                } else
                {
                        message("Matrix not passed in so not used")
                        return(NULL)
                }
        }
        ## function to get original matrix
        get <- function() x
        ## function which sets the matrix calculated outside this object (why?)
        setInverse <- function(inverseAsArgument) inverse <<- inverseAsArgument
        ## function to return the inverse of x if set
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## this function looks up the inverse of the matrix passed in, or creates it,
## stores it and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## assume that x contains a special list object created in makeCacheMatrix
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("Getting cached inverse for you")
                return(inverse)
        }
        ## so there was no cached inverse...better work it out
        ## get the data back
        matrixToSolve <- x$get()
        ## calculate inverse via solve function
        inverse <- solve(matrixToSolve)
        ## set the inverse now we have calculated it for next time
        x$setInverse(inverse)
        ## return the inverse!
        inverse
}
