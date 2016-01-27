## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function stores a matrix x in memory.
## The cacheSolve function shows the inverse of a matrix if it's in memory.

# Write a short comment describing this function
# The makeCacheMatrix function stores a matrix x in memory.
makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y){
    x <<- y
    inverse <<- c
  }
  get <- function() x
  setinverse <- function(invert) c <<- invert
  getinverse <- function() c
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Write a short comment describing this function
## The cacheSolve function shows the inverse of a matrix if it's in memory.
cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}