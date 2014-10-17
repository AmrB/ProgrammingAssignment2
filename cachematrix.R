# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix function creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve function computes the inverse of the matrix returned by 
# makeCacheMatrix. If the inverse has already been computed it gets the
# cashed result and skips the computation. 
# If not, it computes the inverse and sets the value in the cache via
# setinverse function.

# It's assumed that the supplied matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)}
  else {
    message("there is no cached data in the first run.")
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test run:
 x = rbind(c(1, -1/2), c(-1/2, 1))
 m = makeCacheMatrix(x)
 m$get()

## The first time it runs there's no cache
 cacheSolve(m)

## The second run retrieves value from the cache
 cacheSolve(m)

