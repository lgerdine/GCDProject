## R Programming Week 3 Homework
## Build a function that makes list of functions like the mean exercise
## Build a function that checks cache to see if the inverse of the matrix
## has been created. If not, create the inverse
## Date: 02-16-15 
## Student: Laurel Gerdine

## Make list of functions to calculate inverse of matrix

makeCacheMatrix <- function(x = matrix ()) { 
  inverse <- NULL # set the variable that holds the inverse to empty
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## function to check for inverse in cache and either deliver that or calculate inverse

cacheSolve <- function(x=matrix(), ...) {
  inverse<-x$getsolve()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix, ...)
  x$setsolve(inverse)
  inverse
}

## test matrices to use a d other values to set to test

test_matrix1 <- matrix (data = c(1:4), nrow = 2, ncol = 2)
test_matrix2 <- matrix (data = c(5:8), nrow = 2, ncol = 2)

x <- test_matrix1
y <- test_matrix2

LOF <- makeVector2(x)

cacheSolve(LOF)
