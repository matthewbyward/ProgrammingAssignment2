## The two functions makeCacheMatrix and cacheSolve are used to efficiently 
## calculate the inverse of a matrix and will only do so if the inverse has
## have never been caluclated before, otherwise the inverse value will be 
## retrieved from the cache.

## makeCacheMatrix takes a single argument of type matrix and returns
## a list object comprised of four elements of type function: set, get, 
## setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes at minimum a list object returned by the makeCacheMatrix
## function and then uses the function in the list to check if the inverse of
## the matrix has been calculated and either returns it, or it cacluluates, 
## stores, and returns the matrix inverse

cacheSolve <- function(x, ...) {
        
        # gets the value saved as the inverse of the matrix from the cache
        m <- x$getinverse()
        
        # checks if the inverse has already been calculated, if true then 
        # returns the inverse from the cache
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # gets the value for the matrix
        data <- x$get()
        
        # calculates the inverse of the matrix
        m <- solve(data,...)
        
        # stores the calulcated inverse back into the object in the cache
        x$setinverse(m)
        m
}
