## As requested by the assignment, here are 2 functions that can be used together, so 
## to create a special matrix object, able to store in cache its inverse matrix, and 
## another function that calculates the inverse matrix (the first time, and if original
## matrix has changed), and overwrite the cached saved inverse-matrix. If the inverse
## matrix has already been calculated and original matrix has not changed, the second
## function retrieves the inverse matrix stored in cache.
##
##
## Please feed the function only square, invertible matrix, as we assume all matrices 
## provided are invertible, as explained in the assignment.
##
## The first function (makeCacheMatrix) makes a matrix x and set its inverse as NULL 
## (as explained in the assignment, we assume all matrices provided are invertible); 
## it also sets 4 functions in the makeCacheMatrix environment, which is a subset of
## the Global environment (set - to overwrite the matrix; get - to retrieve the matrix;
## setinverse - to overwrite the cached inverse matrix; getinverse - to retrieve the
## inverse matrix). These 4 functions are also listed as named objects, to call them
## with '$'

makeCacheMatrix <- function(x = matrix()) {
            invmat <- NULL
            set <- function(y) {
            x <<- y
            invmat <<- NULL
          }
         get <- function() x
         setinverse <- function(solve) invmat <<- solve
         getinverse <- function() invmat
         list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }



## The second function (cacheSolve) calls the sub-function 'getinverse' of the previous 
## main function (makeCacheMatrix). If inverse matrix is NULL (either first time calculated,
## or change in original matrix) it calculates the inverse matrix and stores it in 
## cache with 'setinverse', otherwise just prints the cached matrix.


cacheSolve <- function(x, ...) {
          invmat <- x$getinverse()
          if(!is.null(invmat)) {
          message('getting cached inverse matrix')
          invmat
          }
          newmat <- x$get()
          invmat <- solve(newmat, ...)
          x$setinverse(invmat)
          invmat
}
