## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that remembers it's inverse once
## it has been calculated using the cachesolve funcion below.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <-function() x
	setinverse <- function(invmat) m <<- invmat
	getinverse <- function() m
	list(set = set, 
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## Compute the inverse of a matrix in a CacheMatrix object and 
## keeps it internally to avoid future recalculations. 

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
