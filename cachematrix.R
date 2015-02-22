## Caching the inverse of a matrix
##
## This assignment tests the use of the <<- operator to assign a value to 
## an object in an environment that is different from the current environment.
## The below are two functions create a special object that stores a matrix 
## and cache's its inverse.

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## 1.	set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the inverse of the matrix
## 4.	get the inverse of the matrix - first check cache

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<-y
		m <<-NULL
	}

	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function () m
	list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## The following function calculates the inverse of the matrix 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via 
## the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


	m <- x$getmatrix()
	
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrixdata <- x$get()
	m <- solve (matrixdata, ...)
	x$setmatrix(m)
	m
}

## --- test the above --- 
## inpmatrix <- matrix(1:4,2,2)	## build a 2*2 matrix
## inpmatrix				## check what was built
## a <- makeCacheMatrix()		## store makeCacheMatrix in var testmatrix
## a$set (inpmatrix)			
## outmatrix <- cacheSolve(a)	## Call routine to get inverse of matrix
## outmatrix
## inpmatrix %*% outmatrix		## check output to get identity matrix

## --- loopholes ---
## 1) cacheSolve aborts with error if determinant is 0
## 2) cacheSolve returns cache even if matrix is changed 
##    if makeCacheMatrix is not loaded again
 
 
