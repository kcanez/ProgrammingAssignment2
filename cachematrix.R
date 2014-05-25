## Put comments here that give an overall description of what your
## functions do

## Make Matrix

makeCacheMatrix <- function(x = matrix()) 
{
  mInv <- NULL
  set <- function(y)
  {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInverse <- function(mI) mInv <<- mI
  getInverse <- function() mInv
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    temp <- x$getInverse()
     if(!is.null(temp))
     {
       print("returning from cache")
       return(temp)
     }
      tempInv <- x$get()^-1
      x$setInverse(tempInv)
      print("NOT returning from cache")
      return(tempInv)
     
}
