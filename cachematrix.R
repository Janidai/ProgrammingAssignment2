## the functions here are useful to save computation. while performing certain operation and the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize inverse variable to NULL as a placeholder for future value
  
  set <- function(y) {  # Defines a function to set
    x <<- y             # the vector x to a new vector y, 
                        # the superasignment operator <<- is used to assign a value to an object in an environment that is different from the current environment. 
    inv <<- NULL        # and resets the inverse, inv, to NULL
  }
  get <- function() x   # returns the vector x
  setInverse <- function(inverse) inv <<- inverse # sets the inverse, inv to inverse
  getInverse <- function() inv # returns the inverse matrix inv
  
  # returns the 'special vector' containing all of the functions just defined
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## cache solve takes a matrix, and returns its inverse

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {    #if you have the inverse of the matrix interest exists. 
    message("getting cached data")
    return(inv)           #we don't need calculate and we can use the cache variable.
  }
  mat <- x$get()          #otherwise return the matrix
  inv <- solve(mat, ...)  #compute the inverse
  x$setInverse(inv)       #cache the inverse
  inv                     #and return the inverse
}
