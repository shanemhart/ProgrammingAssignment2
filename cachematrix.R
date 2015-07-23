## Put comments here that give an overall description of what your
## functions do
##  The first function MakeCacheMatrix, creats a matrix object with a given matrix.
## this object contains not only the orignal matrix, but another matrix wchich is to be the inverse
## of the original.

## The second function is to solve and return the inverse of the special matrix object created by
## the first function

## Write a short comment describing this function
##  This function creates a special matrix, or object.  Using an orignal matrix, it also is able
## to store another object which is the inverse of the original.  It contains call functions to 
## set the object, as well as get and return the values within

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function first attempts to see if the inverse of the matrix created by the first function exists
## if so then it retuns the solution that was already created.
## if not it then attempts to solve, and set the inverse within the object, and display that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  data <- solve(data, ...)
  x$setInverse(data)
  data
}
