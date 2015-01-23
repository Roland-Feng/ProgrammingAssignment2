## Cache the inverse of a matrix
## 

## the way to cache a matrix(like x) is cacheSolve(makeCacheMatrix(x))

makeCacheMatrix <- function(x = matrix()) {
  ive<-NULL
  set <- function(y){
    x <<- y
    ive <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) ive <<- solve
  getinverse <- function() ive
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ive<- x$getinverse()
  if(!is.null(ive)){
    message("getting cached data")
    return(ive)
  }
  data<- x$get()
  ive<- solve(data,...)
  x$setinverse(ive)
  ive
        ## Return a matrix that is the inverse of 'x'
}
