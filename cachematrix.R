## Put comments here that give an overall description of what your
## functions do

## Function to caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)){
    stop("Error: you have to provide a matrix")
  }
  matrix_inverse <- NULL
  set <- function(y){
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  # Inversing the matrix using build in solve() function in R
  setinverse <- function(solve) matrix_inverse <<- solve
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to retrieve cached inverse of a matrix

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data)
  x$setinverse(matrix_inverse)
  return(matrix_inverse)
}
