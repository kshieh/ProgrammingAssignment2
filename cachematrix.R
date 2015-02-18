## Functions to cache the inverse of a matrix, which can be a time-consuming
## calculation. When the inverse is needed again, it can be looked up in the
## cache instead of recomputed. These functions take advantage of the scoping
## rules in the R language to preserve state inside of an R object.

## Creates a special "matrix," which is a list containing a function to:
##   1. Set the value of the matrix
##   2. Get the value of the matrix
##   3. Set the value of the inverse
##   4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv  <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of the special "matrix" created with the above
## function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data using the
## solve function and sets the value of the inverse in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
