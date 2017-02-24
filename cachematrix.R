## I've written two functions: makeCacheMatrix and cacheSolve. makeCacheMatrix takes a matrix as an argument and returns a list.
## The list itself contains 4 functions: set, get, setinverse and getinverse.

## The other function, cacheSolve, is meant to take a list created by makeCacheMatrix. cacheSolve will check to see if 
## the inverse of the matrix in makeCacheMatrix has already been computed and stored in the function getinverse().

## makeCacheMatrix takes a matrix and returns a list containing 4 functions. Anytime a new matrix is passed into X, m is reset as well.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        

}


## cacheSolve takes a list returned by makeCacheMatrix. It checks to see if the inverse of the original matrix has
## been set already and stored by the function getinverse(). If so, it returns the value stored. If not, it is computed and
## set. If this function is ran twice, the first time will compute the inverse and the 2nd time will retrieve it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
