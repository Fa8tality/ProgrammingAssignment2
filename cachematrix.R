## Put comments here that give an overall description of what your
## functions do

## R Programming Assignment 2: Caching the Inverse of a Matrix
##
## The first function: makeCacheMatrix function, creates a special "matrix",
## which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverseMatrix <- function(Inverse) m <<- Inverse
	getInverseMatrix <- function() m
	list(set=set, get=get,
        setInverseMatrix=setInverseMatrix,
        getInverseMatrix=getInverseMatrix)
}

## The second function: cacheSolve function, calculates the inverse of the special "matrix"
## from the makeCacheMatrix function.  However, it first checks to see if the inverse has already
## been calculated.  If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverseMatrix function.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverseMatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data, ...)
	x$setInverseMatrix(m)
	m
       
}
