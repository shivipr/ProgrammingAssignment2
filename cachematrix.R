## Matrix inversion is usually a costly computation
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## The following two functions create a special object which stores a matrix and caches its inverse

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) i <<- solve
  getSolve <- function() i
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  i <- x$getSolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  i
  
}
