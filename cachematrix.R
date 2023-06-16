## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) invmat <<- inverse
        getInv <- function() invmat
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## Return a matrix that is the inverse of 'x'
##This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getInv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data,...)
        x$setInv(invmat)
        invmat
}
