## These functions give the inverse of a matrix x and create a list of 4 functions that can be called to
## set the matrix, set its inverse, get the matrix from cache and get the matrix's previously computed inverse 
##from cache instead of computing it again.


## This function takes the matrix x as an argument and creates a list of 4 functions that can be called to
## set the matrix, set its inverse, get the matrix from cache and get the matrix's previously computed inverse.
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL 
        set <- function(y) {
                x <<- y
                Inv <<- NULL 
        }
        get <- function () x
        setInv <- function(Inv2) Inv <<- Inv2
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of the matrix given to tht makeCacheMatrix function above. 
## If the inverse has already been computed, it gets the inverse from the cache and skips computation.
## Otherwise, it computes the inverse. 

cacheSolve <- function(x, ...) {
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        Matrix <- x$get()
        Inv <- solve(Matrix, ...)
        x$setInv(Inv)
        Inv
}
