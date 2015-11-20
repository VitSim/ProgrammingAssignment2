## This script consists of two functions that are able to cache 
## the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. Consist of:
## Set (1) and get (2) the value of the matrix
## Set (3) and get (4) the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
      # this step stores the cached value and initialize to NULL
        
        set <- function(z) {
                x <<- z
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        # getting the value of the matrix (line 19)
        # invert the matrix and store in cache, called inver (line 20)
        # get the inverted matrix from inver
        # return back the created functions

}


## The function cacheSolve computes the inverse of the "matrix" 
## created earlier with makeCacheMatrix. It first checks if
## the inverse of the "matrix" has already been calculated.
## If this "matrix" has already been calculated (and the 
## matrix is identical), then it gets the inverse from the cache.
## If not, it calculates the inverse of the data.


cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # x is the output of makeCacheMatrix()
        inver <- x$getInverse()
        
        # if this "matrix" has already been calculated
        # get it from the cache and skips the analysis
        if (!is.null(inver)) {
                return(inver)
        }
        
        # if not, calculate the inverse and set the value
        # of the inverse matrix in the cache (using setinv)
              matr <- x$get()
              inver <- solve(matr, ...)
              x$setInverse(inver)
              return(inver)
  }
