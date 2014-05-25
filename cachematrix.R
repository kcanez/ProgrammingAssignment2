## makeCacheMatrix can create a matrix, set matrix values, get matrix, get the inverse matrix, and set the inverse matrix

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


## cachesolve can solve for the inverse of matrix x. it will also cache the value so it does not have to calculate each time function is called

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
