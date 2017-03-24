## The below two function help create a special matrix that can cache its inverse and return the previously calculated inverse in order to prevent redundancy in calculation.

## Function "makeCacheMatrix" creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<- function(y){
		x<<- y
		m<<- NULL
	}
	get<- function()x
	set_inverse<- function(solve) m<<- solve
	get_inverse<- function() m
	list(set = set, get = get, set_inverse = set_inverse,
			 get_inverse = get_inverse)
}


## This function computes the inverse of the special matrix returned by above function.

cacheSolve <- function(x, ...) {
        m<- x$get_inverse()
	if(!is.null	(m)){
		message("Getting Cached Data")
		return(m)
	}
	data<- x$get()
	m<- solve(data, ...)
	x$set_inverse(m)
	m
}
