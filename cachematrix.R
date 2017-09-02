## Hopefully it does what it is supposed to do, creating a matrix 
## through makeCacheMatrix, has the properties get, set, setInverse
## and getInverse. e.g.:
## Matrix1 <- makeCacheMatrix(matrix(c(1, -0.25, -0.25, 1), 2, 2))

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m))
  {
    message("Getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
