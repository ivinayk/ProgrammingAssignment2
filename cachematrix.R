##
## Function to do the following
## Create a special matrix that can remember its inverse, once calculated
## If inverse has not been calculated, calculate and return it;
## If inverse exists (already calculated), then just return it
##

## Function to create the special matrix, described above

makeCacheMatrix <- function(x = matrix()) {
	## Define place holder for the inverse of the matrix
	invx <- NULL
	
	## function to set / initialize the values of the matrix and the inverse
	## whenever a new matrix is specified, also reset the inverse to NULL
	set = function(y)
	{
		x <<- y
		invx <<- NULL
	}
	
	get = function() x
	setInv = function(theInv) invx <<- theInv
	getInv = function() invx
	
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function to derive the inverse, if not already done, and return it
## If inverse already derived, then return it

cacheSolve <- function(x, ...) {
	theInv = x$getInv()
	if(!is.null(theInv))
	{
		message("Returning cached inverse")
		return(theInv)
	}
	
	message("Calculating inverse")
	theInv = solve(x$get(), ...)
	x$setInv(theInv)
	theInv
}
