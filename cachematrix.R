## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix make a new R object for matrix. this function contain 4 function,
## that is for get the matrix, set the matrix, get the inverse of that matrix, and 
## get it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix(NA, nrow(x), ncol(x))
  setValue <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getValue <- function() x
  setInverse <- function(invers) inv <<- invers
  getInverse <- function() inv
  list(setValue = setValue, getValue = getValue, setInverse = setInverse, getInverse = getInverse)
}



# this function is responsible to compute the inverse of the matrix. if the inverse
# has been computed previously, it will retrieve that value. here i am using is.na, and not
# is.null because i make the default value of inverse matrix as NA matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.na(inv[1,1])) {
    print('retrieving cache...')
    return(inv)
  }
  
  value <- x$getValue()
  inv <- solve(value)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
