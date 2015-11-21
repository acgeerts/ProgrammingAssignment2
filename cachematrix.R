## cachematrix.R, (AG) 2015-11-21
## File contains functions for creating a special matrix object 
## that can cache the inverse of the matrix after calculation.

## function makeCacheMatrix creates a special matrix object
## that stores a matrix and caches the matrix inverse
## the function returns a list containing 4 functions
## - set to set the matrix value
## - get to get the matrix value
## - setinverse to set the matrix inverse
## - getinverse to get the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
	## set stored inverse to NULL
	inv <- NULL

	## declare set function for matrix that sets matrix value
	## and resets inverse to NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## declare get function for matrix that returns the matrix
	get <- function() x

	## declare setinverse function that sets matrix inverse
	setinverse <- function(inverse) inv <<- inverse

	## declare getinverse function that returns stored matrix inverse
	getinverse <- function() inv

	## returns list item containing the functions declared above
	list(set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}


## function cacheSolve returns the inverse of special matrix object
## created by makeCacheMatrix.
## The function tries to get inverse from cache. If cached value
## does not exist, the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	## call getinverse to get inverse from cache
	inv <- x$getinverse()

	## if cached inverse exists then return cached data
	if(!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}

	## cached inverse does not exist
	## calculate inverse using solve function
	data <- x$get()
	inv <- solve(data, ...)
	
	## cache the inverse
	x$setinverse(inv)

	## return the inverse
	inv
}
