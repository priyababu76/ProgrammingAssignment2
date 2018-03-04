## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 
## functions created to cache the inverse of matrix
         
## set and get the values of the matrix 
## set and get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set <- function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<- function() m
  list(set=set,setinverse=setinverse,get=get,getinverse=getinverse)
  
}


## Inversing the matrix, if it is already inversed, getting from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
     message("getting cached data")
     return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
    
}
