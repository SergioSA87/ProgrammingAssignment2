## The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
    # 1)set the value of the matrix
    # 2)get the value of the matrix
    # 3)set the value of the inverse
    # 4)get the value of the inverse

## The following function,cacheSolve, calculates the inverse ofa matrix created with the makeCacheMatrix function. 
## It first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of matrix and sets the value of the inverse in the cache via the setInverse function.

## makeCacheMatrix creates a special list containing the function set/get matrix and set/get inverse


makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse property
    i <- NULL
    # Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    # method the get the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    # method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    # method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    # return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function calculates the inverse ofa matrix created with the above function. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    # Get the matrix from our object
    data <- x$get()
    # Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    # Set the inverse to the object
    x$setInverse(m)
    # Return the matrix
    m
}
