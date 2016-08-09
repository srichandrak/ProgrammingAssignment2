                                        #Assignment2
## caching the inverse of a matrix

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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {         #checking whether the cache is null or not 
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) #function to give inverse of a matrix
        x$setinverse(i)       
        i
}

