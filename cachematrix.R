##This first fxn will take a matrix argument and store it, allowing for further manipulation later.
makeCacheMatrix <- function(mat1 = matrix()) {
 
  inv <- NULL
  set <- function(matrix) {
    mat1 <<- matrix
    inv <<- NULL
  }

  get <- function() mat1
  
  set.inverse <- function(setinv) inv <<- setinv
  
  get.inverse <- function() inv
  
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

##The second fxn takes the first fxn's manipulation of the original matrix and returns the inverse, storing it if it has not been calculated yet, and drawing on that stored value if it has been. 
cacheSolve <- function(cachematrix, ...) {
  
  inv <- cachematrix$get.inverse()
 
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
 
  matrix2 <- cachematrix$get()
  inv <- solve(matrix2, ...)
  cachematrix$set.inverse(inv)
  
  inv
}
