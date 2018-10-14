#this code compute the inverse of a matrix x. It saves time because the inverse of a given matrix
#can be stored in the cache, which spare computation time if the inverse of a matrix has already been computed. Only works for invertible matrix

#this function creates a matrix that can be put in a cache
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solvem) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#this function compute the inverse of a matrix x. If it has already been calculated, the matrix is
#retrieved from the cache and returned. Otherwise it is calculated and the result put in the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

