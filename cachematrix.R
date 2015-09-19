# Functions makeCacheMatrix and cacheSolve are based on the makeVector and cachemean functions form Coursera R Programming
# course's 2nd programming assignments

# makeCacheMatrix is a function creating 4 functions - get, set, getinverse and setinverse
# It accepts one argument - x - which should be an invertible matrix

# get function - returns the x matrix
# set function - replaces x matrix with another matrix passed as argument
# set inverse - caches a function argument, which should be an inverse of the matrix. When other argument is passed,
#   it will also be cached (resulting in wrong output of getinverse function)
# getinverse - function retrieves the cached value, which is an inverse of the matrix. Cached vlaue is stored in inv variable


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Funcion takes as an argument a makeCacheMatrix() funciton with a matrix specifies as an argument
# e.g. if m is an invertible matrix, and z is an object stroring a function such as z <- makeCacheMatrix(m)
# calling cacheSolve function with z argument, i.e. cacheSolve(z)
# will return an inverse of m matrix

# First, cacheSolve function calls the getinverse function and stores the results in inv variable
# If any value is retrieved, it is printed and the function stops
# If no value is retrieved, function retrieves cached matrix using get funciton and performs matrix inverse calculation
#   using solve function, storing the result in teh inv variable
# In the last step the result is cached using setinverse function and the result is printed in the console


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
