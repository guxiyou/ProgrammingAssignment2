## Set of functions used to save a matrix, calculate it's
## inverse and then, on subsequent calls, return the cached
## inverse matrix

## makeCacheMatrix: stores initial matrix and cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    
    set <- function(init) {
        inverseM <<- NULL
        x <<- init
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inv) {
        inverseM <<- inv
    }
    
    getInverse <- function() {
        inverseM
    }
    
    ## Return special functions
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve: returns the inverse matrix of a given cacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if (!is.null(inv)){
        message("getting cached data")
    }
    else {
        initial <- x$get()
        inv <- solve(initial)
        x$setInverse(inv)
    }
    
    inv  ## Return inverse matrix of 'x'
}
