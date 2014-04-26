## I have written two function which allow you to calculate
## and get Inverse of Matrix. My functions are intelligent 
## in a way that they cache result and if you ask for inverse
## of same unchanged vector you get cached result. 
## Intellligent and smart Inverse function!

## This function create "special Matrix" object which cache its inverse.
## "special Matrix" is a list of function that allow you to set Matrix,
## get Matrix, compute inverse of matrix set previously and get cached
## inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## setting value of matrix 
  set <- function (y)
  {
    x<<-y
    i<<-NULL
  }
  
  ## getting value of matrix
  get <- function() x
  
  ## Setting inverse of Matrix 
  setInverse <- function (inverse) i<<-inverse
  
  ## getting inverse of matrix
  getInverse <- function () i
  
  ## returnin special matrix consist of required function
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
    
    ## Trying to get cached inverse 
    i <- x$getInverse()
    if(!is.null(i)) 
    {
      message("getting cached data")
      return(i)
    }
    
    ## Getting Matrix provided 
    data <- x$get()
    
    ## Computing inverse of Matrix
    i <- solve(data, ...)
    
    ## Caching inverse of Matrix
    x$setInverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
