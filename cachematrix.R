## Sorry for my english, not fluent at all 
## Using the capability of the scoping rule in R, theses 2 functions create a special matrix "object"
## which "contains" its inverse and a function to compute the inverse only if needed (not done yet).

## The MakeCacheMatrix function creates a special "matrix", which is really a list containing a function to
##    set : set the value of the matrix
##    get : get the value of the matrix
##    setInverse : set the value of the inversed matrix
##    getInverse : get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve function computes the inverse of 'x' only if it has not yet been computed. 
## Otherwise it returns the inverse? In all cases it returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
		## test if already computed and break if
		m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		## compute the inverse
        data <- x$get()
        m <- solve(data, ...)
		## save the result
        x$setInverse(m)
		## return inverse of 'x'
        m
}
