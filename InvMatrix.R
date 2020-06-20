makeCacheMatrix <- function(x = matrix()) {
 
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInv <- function(invM) invMatrix <<- invM 
  getInv <- function() invMatrix
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

cacheSolve <- function(x, ...) {
  
  invM <- x$getInv()
  
  if (!is.null(invM)){
    if ( identical( x$get() %*% invM, invM %*% x$get() ) ){
      print("getting cached data")
      return(invM)
    }
  }
  
  data <- x$get()
  invM <- solve(data, ...)
  x$setInv(invM)
  invM
}