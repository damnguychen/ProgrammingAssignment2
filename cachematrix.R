## These two functions are to cache
## the inverse of matrix so that there
## is no need to compute it repeatedly

## makeCacheMatrix function is to create
## a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(i){
    inverse <<-i
  }
  getInverse<-function(){
    inverse
  }
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## cacheSolve function is to compute
## the inverse of the matrix returned 
## by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data, ...)
  x$setInverse(inverse)
  inverse
}
