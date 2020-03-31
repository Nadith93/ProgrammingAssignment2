## Put comments here that give an overall description of what your
## functions do

## This function creates a special object where the inverse
## of a particular matrix can be cached
makeCacheMatrix <- function( mtrix= matrix() ) {
    inverseMtrix <- NULL
    set <- function( matrix ) {
            mtrix <<- matrix
            inverseMtrix <<- NULL
    }
    get <- function() {
    	mtrix
    }
    setInverse <- function(inv) {
        inverseMtrix <<- inv
    }
    getInverse <- function() {
        inverseMtrix
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# calculates the inverse of the matrix object created by makeCacheMatrix.
# If its already created, that will be retrieved from the cache.
cacheSolve <- function(mtrix, ...) {
    invMtrix <- mtrix$getInverse()
    if( !is.null(invMtrix) ) {
            return(invMtrix)
    }
    data <- mtrix$get()
    invMtrix <- solve(data) %*% data
    mtrix$setInverse(invMtrix)
    invMtrix
}
