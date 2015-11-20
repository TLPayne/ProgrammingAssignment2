## The pair of functions below caches the inverse of a matrix.

## This function stores a list of functions that changes the matrix stored in the main function (set),
## returns the matrix stored in the main function (get), stores the value of the input into the variable
## m (setmatrix), and returns it when needed (getmatrix).
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The following function returns the inverse matrix of the entered matrix. First,
## the function checks if the inverse has already been computed. If so, returns the 
## cached value of m. IF there is not cached value, it computes and returns the inverse, 
## and sets it as the cached value. This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## Returns a matrix that is the inverse of 'x'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m 
}
