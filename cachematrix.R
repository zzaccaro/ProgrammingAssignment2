# These functions cache the inverse of a matrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		# hold cached value
		cache <- NULL

		# function to store a matrix
		setMatrix <- function(y) {
				x <<- y
				cache <<- NULL
		}

		# function to get stored matrix
		getMatrix <- function() x

		# function to cache inverse matrix
		setCacheInverse <- function(solve) cache <<- solve

		# function to get cached value
		getCacheInverse <- function() cache

		# return a list of the functions
		list(setMatrix = setMatrix, getMatrix = getMatrix, 
			setCacheInverse = setCacheInverse, getCacheInverse = getCacheInverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve will retrieve
# the inverse from the cache.
cacheSolve <- function(x, ...) {
		# get the cached inverse matrix
        inverse <- x$getCacheInverse()

        # if the cached value is already calculated, return it
        if (!is.null(inverse)) {
        		message("getting cached data")
        		return(inverse)
        }

        # otherwise, get the matrix to be used
        data <- x$getMatrix()
        # calculate its inverse
        inverse <- solve(data)
        # store it in the cache
        x$setCacheInverse(inverse)
        # return the inverse
        inverse
}