## this function will cache the inverse of a matrix

## makeCachematrix will make a special "matrix" thatcan cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache

cacheSolve <- function(x, ...) {
  m<- x$getsolve()## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
    }
  data <-x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}