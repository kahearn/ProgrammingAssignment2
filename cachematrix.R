## There are 2 functions in this R code. The first function creates a special "matrix" object and
## the second function return the inverse of the matrix.

## This function creates a special "matrix" object and cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
## set the value of the new matrix to the cache and set the inverse matrix in cache to null
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
## get the value of the matrix from the cache
  get <- function() x

## set the value of the inverse matrix of the cache matrix into cache  
  setinverse <- function(solve) m <<- solve
  
## get the value of the inverse matrix and return the updated list
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the "matrix" has not changed, then cachesolve 
## returns the inverse from the cache.

cacheSolve <- function(x) 
{
## get the value of the inverse matrix from the cache and return the value of the inverse matrix. 
## If there is no inverse matrix value in the cache, it will calculate the inverse matrix.
  m <- x$getinverse()
  if(!is.null(m))
    {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
## Return a matrix that is the inverse of 'x'
  m
}
