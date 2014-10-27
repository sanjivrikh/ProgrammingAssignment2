
##---------------------------------------------------
## AUTHOR: 
## Description:
##            This function creates a special "matrix" object that 
##            can cache its inverse.
## 
## USAGE
## Test Case: Pass 3 X 3 matrix
## A <- matrix(c( 4, 0, 5,
## +                + 0, 1,-6,
## +                + 3, 0, 4),3,3,byrow = TRUE)


##----------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property placeholder
  i <- NULL
  ## set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##---------------------------------------------------
## AUTHOR:
## Description:
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (

## USAGE
##  cacheSolve(makeCacheMatrix(A)) ## A is a 3x3 matrix(TestCase specific)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  ##Check if the object is available in the cache already, if yes then get the 
  ##cached data else calulate the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  ##calulate the inverse of the matrix
  m <- solve(data) %*% data
  x$setInverse(m)
  
  ##Return object 
  m
  
}

