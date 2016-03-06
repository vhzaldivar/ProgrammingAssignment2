## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function receives a matrix (we assume its a square inversible matrix)
## It sets the values for the matrix and the inverse of the matrix and also
## defines the set and get methods for both the original matrix and
## the inverse (which originally is NULL)

## NB recomended usage x_bis <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv_x <<- i
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

## This function calculates the inverse of a matrix using the solve() function
## We use as parameter the "special" matrix created by the makeCacheMatrix() function
## we try to retreive (get) the cached inversed matrix but if it does not exists
## we use te solve() function to calculate it and store it in inv_x using the
## setinv method.
## The first time we use this function it will calculate the inverse
## from the second time on, in will use the cached data until we change the matrix

## NB: Recommended usage z <- cacheSolve(x_bis)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x<- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinv(inv_x)
  inv_x
}
