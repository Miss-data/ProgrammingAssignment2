## Put comments here that give an overall description of what your
## functions do

## function that creates a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  d<-NULL
  set<-function(k){
    x<<-k
    d<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) d<<-inverse
  getinverse<-function()d
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## function that computes the inverse of the special “matrix”  and retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  d<-x$getinverse()
  if(!is.null(d)){
    message("getting cached data")
    return(d)
  }
  matrix_data<-x$get()
  d<-solve(matrix_data, ...)
  x$setinverse(d)
  d
}