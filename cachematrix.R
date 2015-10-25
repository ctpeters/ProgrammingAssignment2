## The function makeCacheMatrix() allows to create a matrix pseudo-object that
## can cache the matrix itself as well as its inverse.
## The second function, cacheSolve(), returns the matrix inverse, either
## from the cache or from calculation.

# The function makeCacheMatrix creates a list of functions to
# - cache a matrix                      getMatrix()
# - retrieve the cached matrix          getMatrix()
# - cache the inverse of the matrix     setStoredInverse()
# - retrieve the cached inverse         getStoredInverse()

makeCacheMatrix <- function(x = matrix()) {
        storedInverse <- NULL
        setMatrix <- function(mat){
                x <<- mat
                storedInverse <<- NULL
        }
        getMatrix <- function() x
        setStoredInverse <- function(matrixInverse) storedInverse <<- matrixInverse
        getStoredInverse <- function() storedInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setStoredInverse = setStoredInverse,
             getStoredInverse = getStoredInverse)
}


# The function cacheSolve looks at the cached matrix created with
# makeCacheMatrix(), and calculates and caches its inverse, unless it
# is alreday cached (ie, getStoredInverse does not return NULL).
#
# The function then returns the matrix inverse of 'x'

cacheSolve <- function(x, ...) {
        matrixInverse <- x$getStoredInverse()
        if(!is.null(matrixInverse)){
                message("getting chached matrix inverse")
                return(matrixInverse)
        }
        mat <- x$getMatrix()
        matrixInverse <- solve(mat)
        x$setStoredInverse(matrixInverse)
        matrixInverse
}
