## Cache the inverse of a matrix so it does not need to be 
## re-calculated.

## This function takes a matrix and provides a method
## to access the matrix and its inverse. It will store the 
## inverse matrix once its calculated.

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y){
        x <<- y
        invX <- NULL
    }
    get <- function() x
    setInv <- function(inv) invX <<- inv
    getInv <- function() invX
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## this is a function to solve the inverse of a matrix
## if it has already been solved, the cached inverse will
## be returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}
