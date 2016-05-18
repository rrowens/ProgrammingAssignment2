
##This function creates a special "matrix" object that can cache its inverse.  Matrix caching may help your program run faster.

makeCacheMatrix <- function(x = matrix()) {
  
  
  makeinv = NULL
  set = function(y) {
    x <<- y
    makeinv <<- NULL
    
  }
  get = function() x
  setinv = function(inverse) makeinv <<- inverse 
  getinv = function() makeinv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}



cacheSolve <- function(x, ...) {
        
  makeinv = x$getinv()
  
  if (!is.null(makeinv)){
    
    message("getting cached data")
    return(makeinv)
  }
  
   mat.data = x$get()
  makeinv = solve(mat.data, ...)
  
  
  x$setinv(makeinv)
  
  return(makeinv)
}

