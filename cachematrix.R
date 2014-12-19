## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list including the set value, get value, set inverse value and get inverse value of a matrix 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##default value
        set <- function(y) { ##set the value, only run when called by cache functions
                x <<- y
                m <<- NULL
        }
        get <- function() {x} ##get the value
        setinverse <- function(inverse) {m <<- inverse} ## set the inverse value
        getinverse <- function(){m}  ## get the inverse value
        list(set = set, get = get,  ## list all thesse properties
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function calls makeCacheMatrix to return the inverse of the input matrix, if the inverse is not cached, calculate the inverse;otherwise, send a message and return the inverse 
cacheSolve <- function(x, ...) {
      # the input x is an object created by makeVector
    m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
    if(is.matrix(m)&&!is.null(m)) {              # if inverse was already cached (not NULL) ...

        message("getting cached data")  # ... send this message to the console
        return(m)                       # ... and return the inverse ... "return" ends 
                        #   the function cacheinverse(), note
    }
    data <- x$get()        # we reach this code only if x$getinverse() returned NULL
    m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
    x$setinverse(m)           # store the calculated inverse value in x (see setinverse() in makeVector
    m               # return the inverse to the code that called this function

}
