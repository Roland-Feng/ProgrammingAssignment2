## Cache the inverse of a matrix
## 

## the way to cache a matrix(like x) is cacheSolve(makeCacheMatrix(x))

makeCacheMatrix <- function(x = matrix()) {
  ive<-NULL
  set <- function(y){  ##the set function
    x <<- y
    ive <<- NULL
  }
  get <- function() x  ##the get function
  setinverse <- function(solve) ive <<- solve  ## setinverse function
  getinverse <- function() ive    ##getinverse funcion
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ive<- x$getinverse()
  ##judge if the inverse has been calculated
  if(!is.null(ive)){
    message("getting cached data")
    return(ive)
  }
  ##calculate and store the inverse
  data<- x$get()
  ive<- solve(data,...)
  x$setinverse(ive)
  ive
        ## Return a matrix that is the inverse of 'x'
}
