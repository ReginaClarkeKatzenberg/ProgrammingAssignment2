## Put comments here that give an overall description of what your
## functions do: 
## Asst2, "Caching the Inverse of a Matrix...write a pair of functions that cache the inverse 
## of a matrix. 

## Write a short comment describing this function: 
## Asst2, "This function creates a special 'matrix' object that can cache its inverse."

makeCacheMatrix <- function(makeMatrix = matrix()) {
	cacheMatrix <- NULL
	set <- function(y) {
	makeMatrix <<- y
	cacheMatrix <<- NULL
	}
	get <- function() makeMatrix
	setinverse <- function(inverse) cacheMatrix <<- inverse
	getinverse <- function() cacheMatrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Write a short comment describing this function:
## Asst2, "This function computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the matrix
## has not changed), then 'cacheSolve' should retrieve the inverse from the cache."

cacheSolve <- function(makeMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
	cacheMatrix <- makeMatrix$getinverse()
	if(!is.null(cacheMatrix)) {
		message("getting cached data")
		return(cacheMatrix)
	}
	data <- makeMatrix$get()
	cacheMatrix <- solve(data, ...)
	makeMatrix$setinverse(cacheMatrix)
	cacheMatrix
}
