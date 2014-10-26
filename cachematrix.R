## makeCacheMatrix and cacheSolve functions cache the inverse of a matrix

## makeCacheMatrix function creates a matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the matrix
## returned by makeCacheMatrix above. If previously calculated
## retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setinverse(m)
        m
}
