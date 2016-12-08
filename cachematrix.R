## Put comments here that give an overall description of what your
## functions do

## The function of mkeCacheMatrix creates a special "matrix" object that can cache its inverse
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of the inverse
#4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function fo cacheSolve computes the inverse of the special "matrix" retuned by makeCacheMatrix above.
#if the inverse has been already been calculated, the cacheSovle should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
x<-makeCacheMatrix(matrix(c(1,-1,-3,4),nrow=2,ncol=2))
cacheSolve(x)
cacheSolve(x)
