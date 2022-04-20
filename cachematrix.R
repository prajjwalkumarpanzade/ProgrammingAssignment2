 ##name : prajjwal
## github id : prajjwalkumarpanzade

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                           
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument

    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## to refer 
                                                                                  ## to the functions with the $ operator
}


##this function computes inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
