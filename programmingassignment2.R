makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y){
    x <<-y
    
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(xinv) inv <<- xinv
  get_inv <- function() inv
  list (set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is an inverse of x
  inv <- x$get_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$set_inv(inv)
  inv
}
