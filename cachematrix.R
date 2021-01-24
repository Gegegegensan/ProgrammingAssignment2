## Assignment: Caching the Inverse of a Matrix

## This function creates getters of the matrix and inverse matrix and 
## setters of the matrix and inverse matrix that are accessible from outside of the function
## The inverse is returned from from the cache after running cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<-NULL
  }
  
  get <- function() x
  
  setinversematrix <- function(solve) inverse <<- solve
  getinversematrix <- function() inverse
  
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function calculates inverse matrix and set the calculated value onto makeCacheMatrix() function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinversematrix()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinversematrix(inverse)
  inverse
}


## TEST 1
test_matrix1 <- makeCacheMatrix(matrix(1001:1004, 2, 2))
test_matrix1$get()
test_matrix1$getinversematrix() ## should be NULL
cacheSolve(test_matrix1)
test_matrix1$getinversematrix() ## Should return inverse matrix

#TEST 2
test_matrix2 <- makeCacheMatrix(matrix(c(500, 4000, 456, 555), 2, 2))
test_matrix2$get()
test_matrix2$getinversematrix()
cacheSolve(test_matrix2)
test_matrix2$getinversematrix()
