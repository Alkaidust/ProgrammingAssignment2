## A pair of functions that calculate cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL # initial cache of inverse is null
    setmatrix <- function(newmatrix) {
            x <<- newmatrix # store new input matrix
            cache <<- NULL # delete old cache after a new matrix is stored
    }
    getmatrix <- function() x # get the value of stored matrix
    setinverse <- function(inverse) cache <<- inverse # store the calculated inverse in cache
    getinverse <- function() cache # return the stored inverse from cache
    list(setmatrix = setmatrix, getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse)# generate a list of functions after inputting a matrix
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # input the list of functions generated from makeCacheMatrix
    inverse <- x$getinverse() # get cache from the input list
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse) # return the cache value from the list if it's not null
    }
    matrix <- x$getmatrix() # when cache is null, get the matrix value from the list
    inverse <- solve(matrix, ...) # calculate the inverse of this matrix
    x$setinverse(inverse) # store the calculated inverse in cache from the list
    inverse # return the inverse
}

## Use example
# x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2)) # input a matrix, generate a function list
# x$getmatrix() # show this matrix
# x$getinverse() # now there's no cache of inverse of matrix
# 
# cacheSolve(x) # input the list, calculate the inverse of this matrix, store in cache
# cacheSolve(x) # when run second time, the cached inverse is returned
# x$getinverse() # now there is cache of inverse
# 
# x$setmatrix(matrix(5:8, nrow = 2, ncol = 2)) # input a new matrix to the function list
# x$getmatrix() # show the new matrix
# x$getinverse() # cache is null after new input
# cacheSolve(x) # calculate the inverse of new matrix, store in cache
# cacheSolve(x) # when run second time, the cached inverse is returned
# x$getinverse() # now there is cache of new inverse
