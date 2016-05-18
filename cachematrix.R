
##This function creates a special "matrix" object that can cache its inverse.

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
        ## Return a matrix that is the inverse of 'x'
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  makeinv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(makeinv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(makeinv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  makeinv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(makeinv)
  
  return(makeinv)
}
test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

