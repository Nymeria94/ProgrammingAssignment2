## The combination of functions makeCacheMatirx and cacheSolve calculate
## inverse of a matrix and if the inverse of the matrix is already 
## calculated then it cache the value and if the inverse of the
## same matrix is called again it return the cahced value.

## makeCacheMatrix returns list of functions, set, get, setiverse
## and getinverse. set function set new value of matrix x in parent
## environment, get function return the current value of matrix from
## the parent environmentset, setinverse set the new inverse in I_m
## and getinverse return the value of I_m in the parent environment

makeCacheMatrix <- function(x = matrix()) {
    I_m <- NULL
    set <- function(y){
        x <<- y
        I_m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I_m <<- inverse
    getinverse <- function() I_m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns inverse of a matrix. If the matrix inverse is
## already calculated and it is not changed than it retrieve inverse
## from cached value

cacheSolve <- function(x, ...) {
    I_m <- x$getinverse()
    if(!is.null(I_m)) {
      message("getting cached data")
      return(I_m)
    }
    data <- x$get()
    I_m <- solve(data)
    x$setinverse(I_m)
    I_m
}
