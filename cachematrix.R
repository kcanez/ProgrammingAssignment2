## Put comments here that give an overall description of what your
## functions do

## Make Matrix

makeCacheMatrix <- function(x = matrix()) 
{
  mInv <- NULL
  set <- function(y)                          # sets existing matrix 
  {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x                         # prints current matrix
  setInverse <- function(mI) mInv <<- mI      # caches inverse matrix
  getInverse <- function() mInv               # prints cached inverse
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    temp <- x$getInverse()
     if(!is.null(temp))                   # if matrix is cached, use cache
     {
       print("returning from cache")
       return(temp)
     }
      tempInv <- x$get()^-1               # otherwise set and return cache
      x$setInverse(tempInv)
      print("NOT returning from cache")
      return(tempInv)
     
}
