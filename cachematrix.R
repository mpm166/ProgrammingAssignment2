## Caching the Inverse of a Matrix (R Programming Assignment #2)
## 
## The functions below can be used to compute the inverse of a matrix and cache 
## it for future use.
## 


## makeCacheMatrix - Takes a matrix as input and returns a list of functions
## used to get or set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL #matrix inverse
      set <- function(y) { # can be used to reassign matrix to x in the parent environment
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cachesolve - Takes as input the list returned by makeCacheMatrix and returns
## the matrix inverse if already stored in cache, or calculates and stores the
## inverse if it not already stored
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
      }
      m <- solve(x$get(), ...)
      x$setinverse(m)
      m
}
