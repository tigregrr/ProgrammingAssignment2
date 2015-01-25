## Put comments here that give an overall description of what your
## functions do
##
## Given a matrix-x0, the function computes and catches its inverse. So, given a second
## matrix-x rather then compute its inverse right away, first it compares matrix-x with matrix-x0 to be
## equal. If they are equals, return the catched inverse value, and if they are not then compute
## the inverse of matrix-x, saved and return the new value.
##
## Write a short comment describing this function
## To comppute the inverse of a matrix, it must be square and its determinant is different of zero.
## a) A "matrix object" is bult through four functions: set, get, setinv, and getinv.
## b) x and inv are defined in a parent environment using the set function
## c) set and get allow to cache and retrieve the values for the given matrix.
## d) setinv and getinv allows to cache and retrieve the values for the inverse matrix.
  
makeCacheMatrix <- function(x = matrix()){
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x    
  setinv <- function(inv) inv <<- inv 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## Write a short comment describing this function
## e) To determine the time execution, it is used a timing script (time_duration).
## d) Second, it is determined if the value is NULL or not. If it is null, the inverse of the matrix
##    is computed; conversely, the inv is returned. 
  
cacheSolve <- function(x, ...){
  
  start_time <- Sys.time()
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    end_time <- Sys.time()
    time_duration <- end_time - start_time
    print(time_duration)
    return(inv)
  } else {
    message("computing data")
    data <- x$get()
    inv <- solve(data)%*%data
    x$setinv(inv)
    end_time <- Sys.time()
    time_duration <- end_time - start_time
    print(time_duration)
    return(inv)
  }
}
