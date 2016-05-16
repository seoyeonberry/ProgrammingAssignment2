## following functions will compute and cache the inversion of a matrix

## the first function, makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## this function returns a list containing a function to
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        
        inv = NULL
        set = function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## following function checks to see if the inverse of the matrix has already been computed
## if so, it gets the inverse from the cache and skips the computation
## otherwise, it calculates and set the inverse of the matrix via setinv() function.

cacheSolve <- function(x, ...) {
        ## this function returns a matrix that is the inverse of 'x'
        ## 'x' is an output of the function 'makeCacheMatrix()'
        
        inv <- x$getinv()
        
        # if the inverse has already been calculated
        if(!is.null(inv)){
                # get it from the cache and skips computation
                message("getting cashed data")
                return(inv)
        }
        
        # otherwise, calculates the inverse and sets the inverse of the matrix via "setinv()" function
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        x$setinv()
        
        inv
        
        }
