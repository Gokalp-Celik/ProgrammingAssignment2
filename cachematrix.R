## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes x as a matrix parameter and sets all required functions for get and set. 
## Inverse is kept in cache (i) and initialized as NULL

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks cache of the object created by makeCacheMatrix and tries to solve the matrix
## if tne inverse has not been calculated yet. tryCatch statement is put for possible errors such as 
## matrix not being a square one or matrix is singular. this makes the function not to stop in case 
## it is run in a loop to calculate many inverses at once. if matrix is not square or inverse cannot be calculated
## that condition is also kept in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached inverse data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- tryCatch(solve(data), error = function(e) e[1])
  
  x$setInverse(inv)
  
  inv
        
}
