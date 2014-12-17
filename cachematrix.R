## Computing the inverse of a matrix is time-consuming. It is a good idea to 
## cache the inverse if it is needed multiple times. 
## These two functions are used to create a list that stores a matrix and 
## caches the inverse of the matrix.
## 

## The makeCacheMatrix function takes a matrix as an input and creates a list
## object that contains the original matrix and can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)

}


## The cacheSolve function first checks if the inverse matrix has already been cached. If 
## yes, it returns the inverse matrix from the cache. If inverse matrix has not been 
## calculated, cacheSolve uses the "solve" function to compute the inverse of 
## the matrix from the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  m <- x$getsol()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsol(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
