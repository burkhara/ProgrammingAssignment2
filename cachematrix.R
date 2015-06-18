

#makeCacheMatrix: This function creates a special "vector" object
#that can cache its inverse. This "vector" is a list of functions to do the following: 

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverted matrix
#4.  get the value of the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(imatrix) m <<- imatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The following function calculates the inverse matrix of the special "vector"
#created with the above function. However, it first checks to see if the
#inverse matrix has already been calculated. If so, it `get`s the inverted matrix from the
#cache and skips the computation. Otherwise, it calculates the inverted matrix of
#the data and sets the value of the inverted matrix in the cache via the `setmean`
#function.

#If you run cacheSolve() twice, with the same input, then the second time, you will get the message "getting cached data"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) #assuming a square matrix. Though solve(t(data) %*% data) could also be used 
  x$setinverse(m)
  m
}

