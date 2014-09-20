## Caching the inverse of a matrix. The functions create a special object 
## that stores a matrix and caches it's inverse

## Creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	 m <- NULL
        set <- function(y) {                         ##set the value of matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                          ##get the matrix
        setinverse <- function(solve) m <<- solve    ##set the inverse
        getinverse <- function() m                   ##get the inverse
        list(set = set, get = get,                   ##list of 4 functions
             setinverse = setinverse,
             getinverse = getinverse)


}


## Gets the inverse if it's already available, calculates if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
        if(!is.null(m)) {      ## if the inverse already exists, get from cache
                message("getting cached data")
                return(m)
        }
        data <- x$get()        ## if inverse does not exist, compute and put
        m <- solve(data, ...)  ##it in cache
        x$setinverse(m)
        m
}
