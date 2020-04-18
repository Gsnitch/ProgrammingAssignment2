#This function creates a special "matrix" object that would cache its inverse

makeCacheMatrix <- function(x = matrix()) {   
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function()x
  
  setInverse <- function(inverse) minv <<- inverse
  getInverse <- function()minv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix function above
#If the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setInverse(minv)
  minv
}