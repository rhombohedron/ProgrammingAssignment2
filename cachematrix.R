## returns a list of four functions:
##	set: accepts one matrix argument and sets that as the value of the internal matrix
##	     reinitializes stored inverse value to NULL (inverse is not computed until it is needed)
##	get: returns the value of the internal matrix
##	set.inv: accepts one matrix argument and stores that as the value of the inverse of the internal matrix
##	get.inv: returns the stored value of the inverse of the internal matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(mat) {
    		x <<- mat
    		inv <<- NULL
  	}
  	get <- function() x
  	set.inv <- function(inverse) inv <<- inverse
  	get.inv <- function() inv
  	list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}

## cacheSolve accepts one argument x of the type returned by makeCacheMatrix()
## returns the inverse of the matrix stored in x, if the inverse is initialized (not NULL)
## otherwise, computes the inverse of the matrix stored in x,
##    	      initializes the inverse variable in x to the result, and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$get.inv()
  	if(!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  	}
  	mat <- x$get()
  	inv <- solve(mat)
  	x$set.inv(inv)
  	inv
}