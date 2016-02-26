## These functions can be used to create a matrix, get and set its value(s), and 
## calculate the inverse of the matrix. The inverse is cached, therefore
## making future calculations unnecessary. If the matrix is changed using set(),
## the cached inverse will be flushed.

## Create a matrix object (in the form of a list) e. g.:
## mat <- makeCacheMatrix(matrix(sample(1000000),1000,1000))
## Use mat$get() to get the matrix value(s)
## Use mat$set() to set new value(s)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverted) inv <<- inverted
	getinv <- function() inv
	
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)	
}


## Takes a cacheable matrix created by makeCacheMatrix and solves it, caching
## the resulting inverted matrix in the matrix list object.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
