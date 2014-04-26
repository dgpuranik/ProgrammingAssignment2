## I have written two function which allow you to calculate
## and get Inverse of Matrix. My functions are intelligent 
## in a way that they cache result and if you ask for inverse
## of same unchanged vector you get cached result. 
## Intellligent and smart Inverse function!

## This function create special object which cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y)
  {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setInverse <- function (inverse) i<<-inverse
  getInverse <- function () i
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function computes the inverse of matrix returned by
## makeCacheMatrix above. If the inverse has already been 
## calculated, then this function retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) 
    {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
