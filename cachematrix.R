## The functions contained in this git allow the user to cache
## the inverse of a matrix for retrieval upon command.

## This function (makeCacheMatrix) creates a list of functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse,
             getinverse=getinverse)
}

## This function ("cacheSolve") checks to see if the inverse has
## already been calculated. If so, it uses the calculated inverse;
## if not, it solves for the inverse using the setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(x)
        x$setinverse(inv)
        inv
}
