## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function to set the value of the special matrix, get the value of the matrix, set the value of
# the inverse matrix and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#Function first checks if the inverse of the matrix created with the above makeCacheMatrix() 
#has been computed. If so it will get the inverse from the cache and skip computation
#otherwise it computes inverse matrix of the matrix and sets the value of the inverse matrix
#with the setinverse() function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting inverse of matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}


### test function ## 
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) #intial data
aMatrix <- makeCacheMatrix(m1) #initate makeCacheMatrix
aMatrix$get()               # retrieve the value of x
aMatrix$getinverse()           # retrieve the value of m, which should be NULL

cacheSolve(aMatrix)          # get inverse of original input matrix (m1)
aMatrix$getinverse()           # retrieve it directly, now that it has been cached

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2) #new data
aMatrix$set(n2) # reset value with a new data
# and obtain its inverse by
cacheSolve(aMatrix)
