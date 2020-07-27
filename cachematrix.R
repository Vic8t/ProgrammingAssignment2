## These two functions are used to create a special object
# that stores a matrix and caches its inverse to gain computation time

## makeCacheMatrix creates a special matrix which is a list containing functions
# 1. "set" to set the value of the matrix
# 2. "get" to get the value of the matrix
# 3. "setinverse" to set the value of the inverse
# 4. "getinverse" to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of the special matrix x.
# If the inverse has already been calculated, the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
    }
    else{
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
    }
    inv
}
