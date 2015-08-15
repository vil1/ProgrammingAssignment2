## makeCacheMatrix builds a "special" matrix that caches its inverse
makeCacheMatrix <- function(internalMatrix = matrix()) {
    # Initialize the cached inverse to a NULL value
    inverse <- NULL
    
    # setMatrix updates the internalMatrix and discards the cached inverse
    setMatrix <- function(newMatrix) {
        # the update is performed only if the newMatrix is different from
        # the internalMatrix. This way we avoid to recompute the inverse if 
        # the newMatrix is equal to the internalMatrix (see the example session bellow)
        if(!all.equal(internalMatrix, newMatrix)) {
            internalMatrix <<- newMatrix
            inverse <<- NULL
        }
    }
    
    # getMatrix returns the current value of the internalMatrix
    # The internal matrix would not be accessible otherwise
    getMatrix <- function() internalMatrix
    
    # setInverse caches the given inverse
    setInverse <- function(i) inverse <<- i
    
    # getInverse returns the cached inverse
    getInverse <- function() inverse
    
    # Finally, we return a list containing the four above functions
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## cacheSolve computes the inverse of a given "cacheMatrix" (given by the makeCacheMatrix 
## function above). If the inverse have already been computed and cached, the cached inverse 
## is returned immediatly, otherwise the inverse is computed and cached before being returned.
cacheSolve <- function(cacheMatrix, ...) {
    # First, we grab the cached inverse from the cacheMatrix      
    inv <- cacheMatrix$getInverse()
    
    # If there is a non null inverse, we simply print a message
    if(!is.null(inv)) {
        message("Returning the cached inverse")
    } 
    # Else, we compute the inverse and cache it in the cacheMatrix
    else {
        mat <- cacheMatrix$getMatrix()
        inv <- solve(a = mat, ...)
        cacheMatrix$setInverse(inv)
    }
    # Finally we return the inverse
    inv
}


## Here is an example session using the above functions 
# > mat <- matrix(c(2, 2, 3, 2), 2, 2)
# > mat
#      [,1] [,2]
# [1,]    2    3
# [2,]    2    2
# > cmat <- makeCacheMatrix(mat)
# > cacheSolve(cmat)
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(cmat)
# Returning the cached inverse
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cmat$setMatrix(mat)
# > cacheSolve(cmat)
# Returning the cached inverse
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
