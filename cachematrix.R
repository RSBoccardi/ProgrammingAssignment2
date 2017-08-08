## This function creates a special “matrix” object that can cache its inverse.
## it uses <<- assignment operator so that internal variables are not exposed to the outside environment. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x  # return the input matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## It returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # get the inversed matrix from object x
  if(!is.null(inv)){ # if the inversed result is here
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # if not, get the matrix object
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
