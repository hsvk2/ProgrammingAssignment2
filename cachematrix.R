##The following two functions will calculate the inverse of a matrix and store it in the cache so when it is 
##called again then it will use the stored inverse and not recalculate it. (While the matrix remains unchanged.)


##makecacheMatrix is a function that will create an object that can cache its inverse using cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
 
  ##set is a function to assign y to x in all parent envirments and reset inverse matrix to NULL
  set <- function(y) {    
    x <<- y
    inv <<- NULL
  }
  
  ## get is a function that returns matrix x
  get <- function() return(x)
  
  ## setinv is a function to set the inverse matrix to inv in all parent enviroments
  setinv <- function(inverse) inv <<- inverse
  
  ## getinv is a function to return the inverse
  getinv <- function() return(inv)
  
  ## 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## CacheSolve is a function that will calculate the inverse of the matrix made in the function makeCacheMatrix
##If the inverse has already been calculated and the matrix remains unchanged then the function will 
##retrieve the inverse from the cache without recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
