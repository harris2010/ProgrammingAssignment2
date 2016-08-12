## This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

