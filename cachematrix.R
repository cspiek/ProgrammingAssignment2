## These two functions first create a special "matrix" object that can 
## cache its inverse.  Then second, given an invertible matrix, will
## check if the inversion has already been cached and either return the 
## cached version, or solve for the inversion and cache that inverted
## matrix, then return the result


## This function, makeCacheMatrix(), is the object-making function
## where the inverse is cached

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}  # end makeCacheMatrix




## This function, cacheSolve(), is the check and solve function
## returning the inverse from the cache or solving for it if 
## the cache is NULL

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getSolve()
  if(!is.null(m)) {   # looking for cache flag
    message("getting cached data")
    return(m)
  }  # the cached inverse is returned and skips the solve()
  
  ## else, the solve happens, gets cached, and returns the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
  
}  # end cacheSolve



