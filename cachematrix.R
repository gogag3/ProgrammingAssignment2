## Matrix Inversion is a time taking calculation 
## procedure in any language! If your matrices remain 
## the same, then the following functions help to calculate 	## their Inverse only once and store that value in the cache!
## (if required or called upon for further use in the computation) 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
	x <<- y
	i <<- NULL
	}
	get <- function()x
	setInverse <- function(inv) i <- inv
	getInverse <- function() i
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by ## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
	if(!is.null(i)){
	message("getting cached data")
	return(i)
	}
	a <- x$get()
	i <- solve(a, ...)
	x$setInverse(i)
	i
}

