## Pair of functions, the first creates a matrix object with 
## extra functions attached for storing its inverse in cache.

## $setinverse and $getinverse access the inverse.
## The inverse is cleared to NULL if it has not yet been
## calculated.

## Creates the matrix and attaches the extra functions for 
## accessing the inverse. 

makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
	set <- function(y) 
	{
		x <<- y
		m <<- NULL
	}
	get <- function() 
	{
		x
	}
	setinverse <- function(inverse) 
	{
		m <<- inverse
	}
	getinverse <- function()
	{
		m
	}
	list(set = set, get = get, setinverse = setinverse
		, getinverse = getinverse)
}


## Check the cache to see if the matrix has been inverted, and
## if so, return the cached answer. Otherwise, Calculate and 
## store the result for future access.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m))
		{
			message("getting cached inverse")
		}
		else
		{
			m <- solve(x$get())
			x$setinverse(m)
		}
		return(m)		
}
