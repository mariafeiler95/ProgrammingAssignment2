## makeCacheMatrix caches the inverse of a matrix so you don't have to compute the inverse 
## repeatedly, which takes up time. And time is money!

## The function first sets the values of the matrix, then gets the value of the matrix.
## Then, as you'd guess, it sets the inverse of the matrix, then gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        
}


## This follow up function, cacheSolve, uses the "get" setup from makeCacheMatrix in order to get
## the inverse of the matrix if it has been gotten already. It checks for the inverse and produces
## it if it finds it. If not, it calculates it and sets the inverse for future use. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}
