## Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## x must be a square invertible matrix
      # makeCacheMatrix creates a list containing a function to
      # 1. set the value of the matrix
      # 2. get the value of the matrix
      # 3. set the value of inverse of the matrix
      # 4. get the value of inverse of the matrix
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



# The following function returns the inverse. 
#the function checks if the inverse already exist.
# If it does , then it retrieves from the cache
# else it calculates the ibverse uwing solve funtion
#and return the new calcutaed inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
    # does inverse already exist in cache
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
      # if no inverse exists in cache , calculate the inverse
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
