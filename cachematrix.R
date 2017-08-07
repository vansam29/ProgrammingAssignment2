## makeCacheMatrix and cacheSolve  functions are used to return 
## the inverse of the matrix if it does not exist

## makeCacheMatrix - function is used to get and set the matrix and its inverse values

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  
  setinverse <- function(invm) invmatrix <<- invm
  
  getinverse <- function() invmatrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve - function is used to return the inverse of the matrix if it does not exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached matrix inverse")
    return(invmatrix)
  }
  reamatrix <- x$get()
  invmatrix <- solve(reamatrix)
  x$setinverse(invmatrix)
  invmatrix
}
