## makeCacheMatrix creates a special matrix object, and then cacheSolve
## calculates the inverse of it.
## If the inverse matrix has already been calculated, it will 
## find it in the cache and return it. 

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
        
	get <- function() x
	
	setinv <- function(inverse) inv <<- inverse
	
	getinv <- function() inv
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function cacheSolve returns the inverse of a matrix x created in
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, if
## not, it computes, caches, and returns it. 

cacheSolve <- function(x, ...) {
	
	inv <- x$getinv()
	
	if(!is.null(inv)) {
		message("getting cached inverse matrix")
		return(inv)
	}
        
	else {
		inv <- solve(x$get())
		x$setinv(inv)
	}
	
	inv
}
