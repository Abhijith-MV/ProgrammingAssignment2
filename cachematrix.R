
## makeCacheMatrix: This function creates a 
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(k = matrix()) {
  i <- NULL ## inverse set to null
  set <- function(j) { ## set matrix
    k <<- j 
    i <<- NULL
  }
  get <- function() k ## get matrix
  set_inv <- function(inverse) i <<- inverse ## set inverse of matrix
  get_inv <- function() i ## get inverse of matrix
  list(set = set, ## list of matrices
       get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}
##cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(k, ...) {
  i <- k$get_inv() ## return inverse of k
  if (!is.null(i)) { ## return i if NULL
    return(i)
  }
  data <- k$get() ## get matrices
  i <- solve(data, ...) ## calculate inverse of matrix
  k$set_inv(i) ## set it and forget it
  i
}

## testing is found below 

mat <- (matrix(c(1,3,-1,4),2,2)) ## create test matrix
mat ## return test matrix
inv_mat <- makeCacheMatrix(mat) ## invert test matrix
cacheSolve(inv_mat) ## return inverted matrix

mat1 <- (matrix(c(-2,1,-4,7),2,2)) ## again with new matrix
mat1
inv_mat1 <- makeCacheMatrix(mat1)
cacheSolve(inv_mat1)
