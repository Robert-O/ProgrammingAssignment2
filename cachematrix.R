## Functions to create a matrix (makeCacheMatrix) and get the inverse of the 
## matrix.  Inverse function (cacheSolve) will cache the inverted matrix.

## Creates makeCacheMatrix function takes in a matrix, sets values x (incoming matrix) and
## iMatrix (inverseMatrix).  Returns functions set, get, setinverse,
## and getinverse

makeCacheMatrix <- function(x = matrix()) {
      
      ##  set the imatrix (inverse matrix) value to null
      iMatrix <- NULL
      
      ##  creates the set function.  Not necessary if not calling to set
      ##  explicitly.
      set <- function(y) {
            ## << reaches out to parent environment to set the matrix
            ## and inverse matrix value
            x <<- y
            iMatrix <<- NULL
      }
      
      ## creates the get function to return the matrix
      get <- function() x
      
      ## creates the setInverse function which sets inverseMatrix to the 
      ##  inverse.  Assumes it is explictly passed to the function.
      setinverse <- function(inverseMatrix) iMatrix <<- inverseMatrix
      
      ##  creates the getinvese, returns iMatrix
      getinverse <- function() iMatrix
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


##  creates cachesolve function which takes object created by makeCacheMatrix.
##  Retrieves inverse if it's stored in cache, otherwise calcuates inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ##  retrieve the inverse
      inverse <- x$getinverse()
      
      ## If inverse is not null, use the cached value and return it.
      ## Function would be complete. 
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
      ## if inverse is null, get the value of x (which is original matrix)
      data <- x$get()
      
      ##  calculate the inverse of the data and set it to inverse
      inverse <- solve(data)
      
      ##  sets the cached value of the inverse
      x$setinverse(inverse)
      
      ## return the cached value 
      inverse
}
