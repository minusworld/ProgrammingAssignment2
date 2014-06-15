## This pair of functions is designed to create an 'object' which
## is capable of storing a matrix and its inverse to avoid having
## to recompute the inverse repeadtedly.  makeCacheMatrix(x) takes
## a matrix as an input parameter and returns a list of functions which
## serve as a kind of container for the matrix and its inverse.  
## cacheSolve(x, ...) operates on the list returend by makeCacheMatrix.
## If the makeCacheMatrix does not have the inverse stored, it executes
## solve on the matrix and stores it in the makeCacheMatrix list.  If it
## does have the inverse stored, it simply returns the inverse.

## This function returns a list of functions which serves as a container
## for a matrix and its inverse.  It takes a matrix as a parameter.
## get -> returns the matrix
## set -> sets the matrix and changes the 'originalchanged' status to true
## getinverse -> returns the matrix inverse, if it is set; else NULL
## setinverse -> sets the matrix inverse and changes the 'originalchanged' 
## status to false 
## ischanged -> returns true if the original matrix has been set after
## initialization

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	originalchanged = FALSE
	set = function(y) {
		x <<- y
		originalchanged <<- TRUE
	}
	get = function() x
	## Assumes the inverse is actually correct
	setinverse = function(inverse) {
		inv <<- inverse
		originalchanged <<- FALSE
	}
	getinverse = function() inv
	ischanged = function() originalchanged
	list(set=set, 
		get=get, 
		setinverse=setinverse, 
		getinverse=getinverse,
		ischanged=ischanged)
}


## This function serves as an extension of the solve which utilizes
## the makeCacheMatrix object defined above.  The function checks
## the object to see if it already has the matrix's inverse stored
## and that the original matrix has not changed since it's previous
## computation.  If it does, it returns the stored inverse; if not,
## it calculates the matrix's inverse and stores it in the object.

cacheSolve <- function(x, ...) {
      inv = x$getinverse()
	if (!is.null(inv) & !x$ischanged()) {
		print("retrieving cache")
		return(inv)
	}

	inv = solve(x$get(), ...)
	x$setinverse(inv)
	return(inv)
}
