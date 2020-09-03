## These functions will create a matrix where its matrix can be cached or calculated and it will then either calculate the inverse of the matrix or it will retrieve the inverse from the cache.  

## This function will create a matrix that will allow you to calculate and cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInv <- function(inverse) {inv <<- inverse}
        getInv <- function() {inv}
        list(set, get = get, setInv = setInv, getInv = getInv)
}

## This function will either return the inverse of the matrix or it will retrieve cached data from previous calculations while giving the message "getting cached data". 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}

