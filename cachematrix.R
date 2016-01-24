## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	##
	## This function creates a matrix object and 
	## cache the inverse matrix of it. The function solve
	## is beeing used to create the inverse matrix
	##
	## Following functions are inside: 
	## set: function for setting the matrix
	## get: function for getting the matrix
	## setsolve: function for setting the invers matrix
	## getsolve: function for getting the invers matrix
	##
	
	mi <- NULL  
	 
	set <- function(y) {
		x <<- y
		mi <<- NULL
	}
	
	
	get <- function() x
	
	setsolve <- function(solve) mi <<- solve
	
	getsolve <- function() mi
	
	## returnvalues 
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
		##
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse matrix has already been
        ## computed, than it returs the value from the 
        ## cache and write the message for the user, 
        ## the the cached data have been used. 
        ##
        
        mi <- x$getsolve()
        
        ## checking if the value of the matrix is cached
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi) ## return with the value
        }
        
        ## otherwise compute the inverse matrix
        data <- x$get()
        mi <- solve(data, ...)
        x$setsolve(mi)
        mi
}
