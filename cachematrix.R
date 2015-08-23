## This R Script includes 2 functions
## This function allows to create a special matrix object
##    that can cache its inverse
## Usage example
## > c=rbind(c(1, -1/5), c(-1/5, 1))
## > cc <- makeCacheMatrix(c)
## > cacheSolve(cc)
## > cacheSolve(cc)

## makeCacheMatrix function takes a matrix as argument
##    and create a special matrix object that includes 4 functions
## Usage example
## > c=rbind(c(1, -1/5), c(-1/5, 1))
## > cc <- makeCacheMatrix(c)

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  ## define the set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## define the get function
  get <- function() x
  
  ## define the setinverse function
  setinverse <- function(inverse) m <<- inverse
  
  ## define the getinverse function
  getinverse <- function() m
  
  ## returns the 4 functions as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function computes the inverse of a matrix
## if the inverse is not already in cache, the function
##    computes it, stores it in cache and returns it
## if the inverse is already present in cache, the function
##    retrieves the inverse from cache and returns it
## Usage example
## > cacheSolve(cc)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ## check if the inverse is already in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if the inverse is not already in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
