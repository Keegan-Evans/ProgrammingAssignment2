## Solving the inverse of matrices can be a computing intensive process
## for an individual, invertable matrix that we may need to know the inverse of 
## multiple times, it can be useful to only solve it once, then store the inverse
## so that it can be called later. These functions take a matrix, check to see 
## if its inverse has already been found, if not then solve for the inverse and 
## store it so that it can be retrieved later. If it has been found then it
## simply returns the inverse.

## This function stores a matrix(or creates a blank one to avoid throwing an error), creates a variable
## to store its inverse, and creates 4 functions to work with the matrix in conjuction with the
## cacheSolve function to find and cache the invers of the function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set does the same as the intial makeCacheMatrix but does it from the cacheSolve fuction when called
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  #returns the matrix
  get <- function()x
  #solves the inverse of the matrix so that it is stored inside the environment that is the makeCacheMatrix function
  setinverse <- function(solve) i <<- solve
  #returns the inverse
  getinverse <-function()i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function looks to see if the inverse of the matrix has already been found.
## if it has been, it returns the inverse(which exits the function, preventing it being calculated again)
## if not, then it calculates it by calling get() to retrieve the original matrix, solving it using solve() 
## then storing it in the getCacheMatrix environment

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        
}
