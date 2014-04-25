## Put comments here that give an overall description of what your
## functions do

#The functions below computes the inverse of a matrix and caches the result. When the
# function is called to compute the inverse of a matrix and if the contents of the matrix haven't 
#changed then the function would return the cached value, else it would compute the inverse of the
#matrix, return the result and also the store the result for future use.


## Write a short comment describing this function
#The function `makeCacheMatrix` creates a special "vector", which is
#really a list containing a functions to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of the matrix
#4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #Function sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Function gets the value of the matrix
  get <- function() x
  
  #Function sets the value of the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  #Function gets the value of the inverse of the matrix
  getinverse <- function() m
  
  #Creates list of functions to set, get values, set and get inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function

#The following function calculates the inverse of the special "vector"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the inverse in the cache via the `setinverse`
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
