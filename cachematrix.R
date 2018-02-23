## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
##'mat1' is the sample matrix that user will provide
makeCacheMatrix <- function(mat1 = matrix()) {
inv <- NULL
        set <- function(mat2) {
                mat1 <<- mat2
                inv <<- NULL
        }
        get <- function() mat1
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}




cacheSolve <- function(mat1, ...) {
        ## Return a matrix that is the inverse of 'mat1'
        inv <- mat1$getInv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- mat1$get()
        inv <- solve(data,...)
        mat1$setInv(inv)
        inv

}
