makeCacheMatrix <- function(x = matrix()) { ## sets the function to be a matrix
  makeVector <- function(x = numeric()) { ## creates a numeric vector to store the cache
    inv <- NULL ## create a variable to store the cache and sets it to NULL
    set <- function(y) { ## creates a second function called set with argument y
      x <<- y ## assigns the new matrix y to x in the parent environment
      inv <<- NULL ## (re)sets m to NULL
    }
    get <- function() x ## defines a function 'get' to return the x matrix
    setinv <- function(inv) inv <<- inverse ## Define a function setinv to store the inverse in inv
    getinv <- function() inv ## returns the cached inversed matrix
    list(set = set, get = get, ## returns a list with the 4 functions
         setinv = setinv,
         getinv = getinv)
  }
cachesolve <- function(x, ...) {     ## gets cache data
    inv <- x$getinv()                ## set inv to getinv function
    if(!is.null(inv)) {               ## checks if it is not null / not null = TRUE
      message("getting cached data")  
      return(inv)                     ## if statement is true returns the inverse
    }
    data <- x$get()                   ## defines data as get function
    inv <- solve(data, ...)            ## d 
    x$setinv(inv)                       ## returns a matrix that is the inverse of x
    inv
  }