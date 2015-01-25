## 2 functions that cache and compute
## the inverse of a matrix

## 1: creates a matrix object that can cache the inverse

makeCacheMatrix <- function(mtx = matrix()) {
	inverse <- NULL
	set <- function(x) {
		mtx <<- x;
		inverse <<- NULL;
	}
	get <- function() return(mtx);
	setinv <- function(inv) inverse <<- inv;
	getinv <- function() return(inverse);
	return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## 2: computes the inverse of 'makeCacheMatrix'

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
		if(!is.null(inverse)) {
			message("Getting cached data...")
			return(inverse)
		}
		data <- mtx$get()
		inverse <- solve(data, ...)
		mtx$setinv(inverse)
		return(inverse)
}
