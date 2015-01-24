## These functions work to compute the inverse of a given matrix. The inverted matrix will be
## stored in the cache ready to be called whenever it is needed again.

## The first function, makeCacheMatrix creates a list containing a function to:
##   1.set the value of the matrix
##   2.get the value of the matrix
##   3.set the value of the inverse matrix
##   4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
		
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function returns the inverse of a given matrix. This function checks to see
## if an inverse has already been computed. If an inverse exists, this function gets the value
## and skips the computation. If there is not an inverse, this function will set the value in the
## cache through the setinverse function.

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data,...)
	x$setinverse(inverse)
	inverse
}