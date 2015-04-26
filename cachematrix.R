## The makeCacheMatric and cacheSolve functions are used to cache the inverse of a matrix in order to
## save time and processing by returning a cache of an inverse instead of calculating it again and again.
## It is assumed that the matrix supplied is always invertable. 
## Some example usage:
##      a = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##      a$get() ## returns the matrix
##           [,1][,2]
##      [1,]   1   3
##      [2,]   2   4
##      cacheSolve(a)   ## computes and caches matrix inverse or returns the cached matrix inverse
##           [,1][,2]
##      [1,]  -2   1.5
##      [2,]   1  -0.5
##      a$getinverse()    ## returns matrix inverse
##           [,1][,2]
##      [1,]  -2   1.5
##      [2,]   1  -0.5
##      cacheSolve(a)   ## returns the cached matrix inverse
##      getting cached data
##           [,1][,2]
##      [1,]  -2   1.5
##      [2,]   1  -0.5
##      a$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) ## modifies existing matrix
##           [,1][,2]
##      [1,]  -0.13333333  0.2
##      [2,]   0.01010101  0.0

## makeCacheMatrix() creates the matrix that can cache its inverse and stores a list of functions that:
## 1. set(): set the value of the matrix
## 2. get(): get the value of the matrix
## 3. setinverse(): set the value of the inverse
## 4. getinverse(): get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## initalizes the inverse value to NULL 
  set <- function(y) {
    x <<- y
    ## substitutes x with y in makeCacheMatrix()
    i <<- NULL
    ## restores the null value to the inverse
  }
  get <- function() x
  ## function that returns the matrix input
  setinverse <- function(inverse) i <<- inverse
  ## stores/caches the value of the input (inverse) into variable i
  getinverse <- function() i
  ## returns the stored value for the inverse
  list (set = set, get = get, setinverse = setinverse,getinverse = getinverse)
  #stores the four functions above into the main function
}


## cacheSolve() returns the inverse of the matrix given in makeCacheMatrix().
## If the inverse  already calculated and the matrix has not changed, function retrieves cached inverse.
## If the inverse not in memory, function calculates the inverse of the matrix input.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## sets i to be the value of i in makeCacheMatrix()
  if(!is.null(i)) {
    ## if the value of i exists in memory and the inverse is already calculated then
    message("getting cached data")
    ## will return the message given above
    return(i)
    ## and returns the value of the cached inverse
  }
  data <-x$get()
  ## if the inverse is not in memory, gets the original matrix stored in makeCacheMatrix()
  i <- solve(data, ...)
  ## calculates the inverse
  x$setinverse(i)
  ## sets the newly calculated value of i (the inverse) in the cache
  i
  ## returns the calculated inverse
}
