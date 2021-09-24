## The function makeCacheMatrix creates an R object that stores a matrix and its inverse

## The matrix value is passed as an argument to the function that is used to 
## store the cache of inverse of the matrix which will be calculated in cacheSolve.  

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
              set <- function(y){
                x <<- y 
                inv <<- NULL
                }
              get <- function() x
              setinv <- function(inverse) inv <<- inverse
              getinv <- function() inv
              list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is used to calculate the inverse of matrix and the values are stored in the parent environment

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
