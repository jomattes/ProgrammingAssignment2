## These two functions create a matrix object that can cache its
##inverse so it doesn't have to be constantly recalculated


#creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	#nulls the previously saved inverse
	inv <- NULL
	
	#sets a new matrix to be used
	setm <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	#returns the current saved matrix
	getm <- function() x
	
	#sets the saved inverse to a value
	setinv <- function(inverse) inv <<- inverse
	
	#returns the current saved inverse
	getinv <- function() inv
	
	#stores the functions
	list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
	
}

#calculates the inverse of the matrix made by makeCacheMatrix
cacheSolve <- function(x, ...) {

	#returns cached inverse if available and ends the function
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	#returns the saved matrix, then calculates the inverse
	#and saves it in inv
	data <- x$getm()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}