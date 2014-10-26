## Overall description: The functions calculate the inverse of an invertible matrix
## that is stored in the cache. If the inverse of the matrix has already been calculated, 
## then the inverse should be retrieved from the cache.

## This function creates a special "matrix" object that can cache its inverse
## The function is actually a list, containing functions to
## 1. set the (values of the) matrix
## 2. get the (values of the) matrix
## 3. set the inverse matrix
## 4. get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set.matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  get.matrix <- function() x
  set.inv.matrix <- function(inv.matrix) m <<- inv.matrix
  get.inv.matrix <- function() m
  list(set.matrix = set.matrix, get.matrix = get.matrix,
       set.inv.matrix = set.inv.matrix,
       get.inv.matrix = get.inv.matrix)
}


## This function computes and returns the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then the inverse should be retrieved from the cache
cacheSolve <- function(x, ...) {
  m <- x$get.inv.matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get.matrix()
  m <- solve(data, ...)
  x$set.inv.matrix(m)
  m
}

## This is to check functionality of the functions
a <- makeCacheMatrix(matrix(c(1,4,9,16), nrow = 2, ncol = 2))
cacheSolve(a)
cacheSolve(a)
