## A set of 2 functions that are used to create a special matrix object with cache storage capability
## Another to either check for the cached inverse value or create the inverse and save to cache

## makeCasheMatrix creates a special object of a matrix using functions in order to cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initializing inverse object
  
  set <- function(y) {  #set function
    x <<- y  #assign y to x in parent environment
    m <<- NULL  #assign m to NULL for a reset in parent environment    
  }
  
  get <- function() x  #retrives the input value x which should be a matrix
  
  setInverse <- function(invMatrix) inv <<- invMatrix # sets the solved inverse to our inverse  cache object
  
  getInverse <- function() inv #gets the inverse object that was set previously into cache
  
  
  list(set = set,  
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  #creating a list of the defined functions for object return
  
}


## cacheSolve is used to get the inverse of the matrix object given.
## if it is already cached it returns that, if not it calculates the inverse and saves to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  #get inverse from makeCacheMatrix object
  
  if(!is.null(inv)){ # test to see if inv is already cached 
    message("getting cached data")
    return(inv) #return cached inverse
  }
  
  data <- x$get() #put matrix into data object for computations
  inv <- solve(data,...) #use solve to find inverse
  x$setInverse(inv) #set the result into our object
  inv #return the new inverse matrix
  
}
