## This function defines four properties of a matrix including the set, get, setinverse and getinverse, 
## and return them in a way of list 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##default value
        set <- function(y) { ##store the input matrix in x, only run when called by cache functions
                x <<- y
                m <<- NULL
        }
        get <- function() {x} ##obtain the matrix
        setinverse <- function(inverse) {m <<- inverse} ## set the inverse
        getinverse <- function(){m}  ## get the inverse 
        list(set = set, get = get,  ## list all these properties
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function calls makeCacheMatrix to return the inverse of the input matrix, if the i
## nverse is not cached calculate the inverse;otherwise, send a message and return the inverse 
cacheSolve <- function(x, ...) {
      # the input x is an object created by makeCacheMatrix
    m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
    if(is.matrix(m)&&!is.null(m)) {              # if inverse was already cached (not NULL and is a matrix) ...

        message("getting cached data")  # ... send this message to the console
        return(m)                       # ... and return the inverse ... "return" ends 
                        
    }
    data <- x$get()        # we reach this code only if x$getinverse() returned NULL
    m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
    x$setinverse(m)           # store the calculated inverse value in x (see setinverse() in makeCacheMatrix
    m               # return the inverse to the code that called this function

}
