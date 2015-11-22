## Put comments here that give an overall description of what your
## functions do

## Comments by Rajesh Kumar S
## makeCacheMatrix is a function that is to initialize, set, get and setinverse and getinverse of a matrix
## cacheSolve is a function that gets inverse and it not present in cache, computes the inverse and puts in cache

## Write a short comment describing this function

## Comments by Rajesh Kumar S
## This function first initializes the Inverse of the martix to NULL
## It has the 'set' and 'get' functions to set and get the matrix into it variable x
## setinverse and getinverse set and get the inverse of the matrix stored in inverseMatrix variable
## The <<- operator is the super assignment operator where the assignment is done in the global environment, rather than to a variable that is within a function. That is the reason we are able to 'get' the already 'set' value of the inverse of the matrix from the inverseMatrix variable.
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverseMatrix <<- inverse
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Comments by Rajesh Kumar S
## This function gets the inverse of the matrix passed.
## The useage is to first call makeCacheMatrix passing a matrix and assign to a variable
## Here the inverseMatrix is set to NULL and the matrix is stored in variable x.
## Next is to call cacheSolve and passing teh makeCacheMatrix function (assigned to a variable in the above setp
## If it is the first time, the getinverse returns NULL and so, the matrix is fetched, inverse is calculcated and the inverse is set
## Subsequent call, the getinverse does not return NULL but returns the inverse of the matrix from the cache.
## To change the matrix, the 'set' function of the makeCachleMatrix has to be called. The next call to cacheSolve re-calculates the inverse (since the inverseMatrix is set to NULL). Subsequent call returns inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
