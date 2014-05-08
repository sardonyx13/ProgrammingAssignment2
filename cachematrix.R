## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. Presented 
## functions inverse and cache the inverse of a matrix.

##  'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setinvers <- function(invers) inv <<- invers
	getinvers <- function() inv
	list(set = set, get = get,
		 setinvers = setinvers,
		 getinvers = getinvers)
}


##  'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.
##  If the inverse has already been calculated (and the matrix has not changed), then 
##  'cachesolve' retrievse the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinvers()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinvers(inv)
	inv
}
