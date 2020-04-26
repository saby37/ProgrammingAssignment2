##This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##Here, the argument of the functio is given
  inver<-NULL                    ##Here, inverse is initialized as null
  set<-function(y){
    x<<-y
    inver<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) 
    inver<<-inverse
  getinverse<-function()inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function gives the inverse of "matrix".
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

  cacheSolve<-function(x,...){
    inver<-x$getinverse()
    if(!is.null(inver)){
      message("getting cached data")
      return(inver)
    }
    data<-x$get()
    inver<-solve(data,...)
    x$setinverse(inver)
    inver
    
  }
  