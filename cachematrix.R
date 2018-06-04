## Below are two functions that used to calculate inverse of a matrix,
## if the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by function above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}