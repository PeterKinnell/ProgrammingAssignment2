## These two functions work together to allow the inverse of a matrix to be calculated (cacheSolve) and stored for later use (makeCacheMatrix). 

## This function defines an object that is used to store the a cached inverse matrix or the matrix passed to the function in the function call. It returns a list of functions that may be used to either set the function with a new matrix that needs to be inverted, set the cached inverted matrix, get the value of the submitted matrix before any inversion, or get the cached inverted matrix. 



makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvm <- function(invmat) invm <<- invmat
        getinvm <- function() invm
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}










## cachesolve takes the input to the function which is an makeCachMatrix object, and if a previous inverse matrix has beed cached within this object, then this value is returned, if it has not then the function makes use of the solve() function to return the inverse to the matrix. The resutls is stored in the mackCacheMatrix object, and also retuned. 

cacheSolve <- function(x, ...) {
        invm <- x$getinvm()
        
	if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        
	data <- x$get()
        invm <- solve(data, ...)
        x$setinvm(invm)
        invm
}
