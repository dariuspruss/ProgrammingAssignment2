## This script contains a pair of functions that allow you to cache the inverse of a matrix, calculated using the solve() function.

## The makeCacheMatrix function creates a matrix object that is able to cache its inverse. 
## Once this function is called, e.g. via "> amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))", 
## you can use the functions "$get()", "$set()", and "$getinverse()" to manipulate your data. For example, "> amatrix$get()" will return the original matrix. 
## NOTE: you shouldn't directly call "$setinverse()".

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {                          ## creates a function to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                           ## creates a function to get the value of the matrix
  setinverse <- function(solve) m <<- solve     ## creates a function to set the value of the inverse
  getinverse <- function() m                    ## creates a function to get the value of the inverse
  list(set = set, get = get,                    ## returns a list of objects, in this case the functions we just created
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the value from the "getinverse()" function above. 
## If the inverse has already been cached, it will return the cached data.
## If the inverse hasn't yet been cached, it will cache the calculated value and then return it.

cacheSolve <- function(x, ...) {                
  m <- x$getinverse()                           ## gets the value
  if(!is.null(m)) {                             ## if a value has been cached, that value is returned
    message("getting cached data")
    return(m)
  }
  data <- x$get()                               ## retrieves the original matrix
  m <- solve(data, ...)                         ## calculates the inverse
  x$setinverse(m)                               ## sets the value that has been calculated
  m                                             ## returns the value
}
