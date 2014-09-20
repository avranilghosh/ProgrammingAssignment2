## The functions are used to do matrix operations

## makeCacheMatrix function is used to create a matrix,print the matrix,set its inverse and print its inverse

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


## CacheSolve function is used to calculate the inverse of a matrix if not stored in cache and print it.If Inverse is already stored in cache, it prints the inverse from cache and does not re-calculate

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
}
