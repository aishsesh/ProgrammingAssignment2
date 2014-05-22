## The functions written below can be used to cache the inverse of a matrix in case it has to be calculated repeatedly

## makeCacheMatrix takes a matrix(default value is an empty matrix) and returns a list of 4 functions 
## the get and set functions are used to access and set the matrix and the getinv and setinv to access and set the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #sets matrix and sets inverse to null to remove any previously stored value in inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #returns matrix
  get <- function() x
  #sets inverse
  setinv <- function(inverse) inv <<- inverse
  #returns inverse
  getinv <- function() inv
  #this is the list of 4 functions that is returned
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##cacheSolve returns the inverse of 'x' using the given cacheMatrix (the list returned in the above function)
## it computes the inverse only if the cached value is not present
cacheSolve <- function(x, ...) {
  #accesses the existing value of inverse
  inv <- x$getinv()
  # if a value is present the cached value is returned
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #inverse is calculated as there was no previously computed inverse 
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}
