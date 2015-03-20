## Write a function that creates a matrix object that can cache its inverse
## Then compute the inverse of the matrix object in another function

## Create matrix object as a list containing a function to
## set matrix value
## get matrix value
## set matrix inverse by solve()
## get matrix inverse
makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                      x <<- y
                      m <<- NULL
              }
              get <- function() x
              setmatrix <- function(solve) m <<- solve
              getmatrix <- function() m
              list (set=set, get=get,
                    setmatrix=setmatrix,
                    getmatrix=getmatrix)
}

## Calculate the inverse of matrix object
## Check whether inverse has already been calculated 
## If so, get and return cached data
## Otherwise inverse matrix object and set matrix inverse in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
              m <- x$getmatrix()
              if (!is.null(m)) {
                      message("getting cached data")
                      return(m)
              }
              m.data <- x$get()
              m <- solve(m.data, ...)
              x$setmatrix(m)
              m
}