makeCacheMatrix <- function(x = numeric()) {
  
  # Caching the inverse of a matrix
  # 2nd assignment for the R Programming course
  # makeCacheMatrix creates a list containing functions to
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the value of inverse of the matrix
  # 4. get the value of inverse of the matrix
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

 # The following function returns the inverse of the matrix. It first checks if
 # the inverse has been computed already. If so, it gets the result and skips the
 # computation. Otherwise, it computes the inverse, sets the value in the cache
 # using the setinverse function.

 #
 # This function assumes that the matrix is always invertible.
 # Implementing the invertability test is straighforward  
 #
cacheSolve <- function(x, ...) {
 #  
 # if inv is empty, proceed to computing it
 # if not, raise the message and pick up its value
 #  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
###############################
# 
# TEST    
#
#x <- rbind(c(1, 2), c(3, 4))
#m <- makeCacheMatrix(x)
#m$get()
#        [,1] [,2]
#[1,]    1   2
#[2,]    3   4
#
### no cache 
#>cacheSolve(m)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#
## pick up the valie from the value in the second run
#>cacheSolve(m)
#getting cached data
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
##
