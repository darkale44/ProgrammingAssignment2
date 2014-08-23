## Last edited 23/8/2014

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Example usage
## speshMatrix <- makeCacheMatrix(myMatrix)
## speshMatrix$get()               ## this will print out the contents of myMatrix
## speshMatrix$set(matrix(..,..))  ## this will set the value of myMatrix to whatever is in the argument
## speshMatrix$getinverse()        ## this will print out the inverse of myMatrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSovle computes the inverse of the special "matrix" returned by `makeCacheMatrix` above
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Example usage
## cacheSolve(speshMatrix)      ## this will print out the inverse of myMatrix (as documented above)
##                              ## if the inverse has already been calculated, it will display the message
##                              ## 'getting cached data' before printing the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}