## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL                                    #matrix(data = NA, nrow= i, ncol = j)
        #print(inv)
  }
  get <- function() x                                   #get the matrix
  setinverse <- function(inverse) inv <<- inverse       #set the inverse of a matrix
  #setinverse <- as.matrix(setinverse)
  getinverse <- function() inv                          #get the inverse of a matrix
  #getinverse <- as.matrix(getinverse)
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                 #as.matrix(x["getinverseM"])
  if(!is.null(inv)) {                   #checks to see if the inverse matrix has already been calculated
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                       #x["get"]
  inv <- solve(data, ...)
  #inv <- as.matrix(inv)
  x$setinverse(inv) #x["setinverseM(inv)"]
  inv                                  # Return a matrix that is the inverse of 'x'
}

