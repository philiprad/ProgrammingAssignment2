## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        ## getter and setter methods to allow caching of inverse in the object
        get <- function() x
        
        setInverse <- function(inverse) n <<- inverse
        getInverse <- function() n
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of a matrix or retrieves the inverse 
## of the matrix, if it has alreay been computed, from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getInverse()
        
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }     
        return(solve(x))
}
