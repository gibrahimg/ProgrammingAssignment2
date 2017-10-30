## The purpose of this function is cache matrix inversion calculations so that they can be
## accessed later instead of calculating them repeatedly.

## This function enables the creation of a square inversible matrix
## It returns a list containing functions that set the matrix, get the matrix, set the inverse, and get the inverse.
## It uses <<- to assign values ot objects in environments that are different to their current environments

makeCacheMatrix <- function(x = matrix()) {
        i <- Null
set <- function(y) {
        x <<- y
        i <<- Null
}
get <- function() x
getinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set,
     get = get,
     setinverse = setinverse
     getinverse = getinverse
}

## This function returns the output of makeCacheMatrix defined above
## which is the inverse of the matrix given as input to makeCacheMatrix
## If the inverse has already been calculated, it returns it form the cache
## Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
}
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
