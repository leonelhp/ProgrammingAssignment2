# We will create a function that creat a "special matrix" that allowed you
# to set and get the original matrix, and set, get the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inversa) inv <<- inversa
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The next function calculete the inverse of a matrix and then set it into
## the solve space of the "special matrix"
## Also detect if the inveverse was already calculeted. In that case it will
## cache the solución and show it
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

