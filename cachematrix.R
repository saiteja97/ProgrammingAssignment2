#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.

# This function makeCacheMatrix creates
# a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function () x
  setInverse <- function(inverse) inv_cache <<- inverse
  getInverse <- function() inv_cache
  list( set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


#  Function cacheSolve computes the inverse 
#  of the special "matrix" created by function makeCacheMatrix.
#  If the matrix has not change and the inverse 
#  has already been calculated, 
#  function retrive the inverse from chache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inv_cache <- x$getInverse()
  if (!is.null(inv_cache)) {
      message("Getting inverse from chache")
      return(inv_cache)
  }
  # create matrix since it does not exist
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  return(inv)
}


###########################
# Simple test:

# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

# > my_matrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# Is there cached inverse for that matrix?

# > my_matrix$getInverse()
# NULL

# calculating inverse:

# > cacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Is there cached inverse for that matrix?

# > cacheSolve(my_matrix)
# Getting inverse from chache
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > my_matrix$getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
