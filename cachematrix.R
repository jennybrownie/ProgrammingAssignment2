## These two functions create a matrix object that can cache its
## inverse and return the cached value.  Having a cached value
## available saves recalculating time.


## makeCacheMatrix creates the special matrix object that will be
## used by the other function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #returns matrix
  setinverse <- function(inv) m<<- inv #sets inverse matrix value
  getinverse <- function () m #returns inverse matrix
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function computes the matrix inverse, or returns the cached
## inverse matrix, when available

cacheSolve <- function(x, ...) {
  m<- x$getinverse()
  if(!is.null(m)){ #if inverse is already cached
    message("getting cached data")
    return(m) #return cached value
  }
  data <-x$get() #otherwise get the matrix so we can calculate the inverse
  m<-solve(data)
  x$setinverse(m) #pass setinverse the matrix calculated on line above
  m   ## Return a matrix that is the inverse of 'x'
}
