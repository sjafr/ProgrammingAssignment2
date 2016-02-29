##
## makeCacheMatrix: This function creates a special "matrix" object that can
##cache its inverse.

##
##Computing the inverse of a square matrix can be done with the solve function
##in R. For example, if X is a square invertible matrix, then solve(X) returns its
##inverse.


makeCacheMatrix <- function(xx = matrix()) {
        inverse_matrix <- NULL
        set <- function(yy) {
                xx <<- yy
                inverse_matrix <<- NULL
        }
        get <- function() xx
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



##2.    cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been
##calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.
##
## assumption: The matrix supplied is always invertible
##
##
cacheSolve <- function(xx, ...) {
        inverse_matrix <- xx$getinverse()
        if(!is.null(inverse_matrix)) {
                return(inverse_matrix)
        }
        data <- xx$get()
        inverse_matrix <- solve(data)
        xx$setinverse(inverse_matrix)
        inverse_matrix
         }

