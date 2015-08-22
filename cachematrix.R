## R Programming, Programming Assignment 2
## These functions can create a cachable matrix and 
##then solve the matrix, using a cached version 
##if appropriate.

## This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(matrx = matrix()) {
  invm <- NULL                                       ##Initializes inverted matrix variable
  
  set <- function(y) {
    matrx <<- y                                      ##set function stores original matrix in matrx variable
    invm <<- NULL                                    ##When a new matrix is stored, the inverted matrix variable is reset to NULL.
  }
  
  get <- function() matrx                            ##get subroutine displays contents of matrix variable
  setinverse <- function(inverse) invm <<- inverse   ##setinverse subroutine sets the inverted matrix variable
  getinverse <- function() invm                      ##getinverse subroutine displays the inverted matrix variable
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)                      ##creates a list as part of makeCacheMatrix, including the various subroutines
}

##This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()                           ##Loads inverted matrix into invmat
  if(!is.null(invmat)) {                             ##If invm is not NULL, then loads cached data
    message("Getting cached matrix data:")
    return(invmat)
  }
  data <- x$get()                                    ##data variable loaded with source matrix file
  invmat <- solve(data, ...)                         ##inverted matrix is calulated and loaded into invmat variable
  x$setinverse(invmat)                               ##inverted matrix is stored
  invmat                                             ##autoprint of inverted matrix
}