## Function "makeCacheMatrix" create "special matrix object", 
## where stored data of original matrix and its inversed matrix (where it will be counting),
## and function cacheSolve retun inverse matrix from saved data, or counting it at first time


## 1) Function create "special matrix object" from the input matrix, 
## and return list of function to get-set matrix and get-set inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## 2) Function or calculates inversed matrix of "special matrix object",  
## or return it from saved data if it already exist

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
        ## Return a matrix that is the inverse of 'x'
}
