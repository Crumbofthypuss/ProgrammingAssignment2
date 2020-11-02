## This function returns the inverse of the matrix provided but it can also
## cache the previous value of the inverse if it has already been solved

## Caches the values of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inversematrix <- NULL
  getmatrix <- function() x
  setinverse <- function(inverse) inversematrix<<-inverse
  getinverse <- function() inversematrix
  list(getmatrix=getmatrix,setinverse=setinverse, getinverse=getinverse)
  
}

## solves for inverse of matrix, if inverse has been computed already, return cached value
## initialmatrix=provided matrix, inversematrix= solved inverse of matrix
cacheSolve <- function(x, ...) {
  inversematrix<-x$getinverse()
  if (!is.null(inversematrix)){
    print("solved, already. Retrieving cached data....")
    return(inversematrix)
  }
  initialmatrix<- x$getmatrix()
  inversematrix<- solve(initialmatrix,...)
  x$setinverse(inversematrix)
  inversematrix
  
}

#NOTE: WHEN DOING THIS HOMEWORK, FOLLOW THE STEPS BELOW
#x<- matrix()
#y<-makeCacheMatrix(x)
#cacheSolve(y)
#-----to verify if value was cached-------
#cacheSolve(y)

