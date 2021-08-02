## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 
## (annotated besides each line)

makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL ##initialize i(nverse) to be NULL
    set <- function(y) { ##set the value of the matrix
        x <<- y
        i <<- NULL ##set i to be NULL again if there is a new matrix
    }
    
    get <- function() x ##return the matrix argument
    
    setinverse <- function(inverse) i <<- inverse ##assign the value to i (instead of NULL)
    getinverse <- function() inv ##return the value of inverse (or NULL)
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function 
## (annotated besides each line)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() ##extract the inverse matrix
    if(!is.null(i)) { ##if i is not NULL
        message("getting cached data") ##print a message
        return(i) ##extract/return the value directly
    }
    data <- x$get() ##if i is NULL, call the matrix argument
    
    i <- solve(data, ...) ##calculate/solve i 
    x$setinverse(i)
    i ##return inverse matrix
}
