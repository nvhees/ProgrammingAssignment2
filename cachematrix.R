## This code is for programming assignment 2, R programming, Johns Hopkins on Coursera
## Answers by Nathalie van Hees, 25 October 2015.


## The function makeCacheMatrix creates a matrix object that can cache its inverse.
 
makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix.
## If a previously calculated inverse matrix was stored in the cache by makeCacheMatrix,
## cacheSolve retrieves and returns the cached matrix, including a message. 
## If the cache is empty, cacheSolve calculates the inverse matrix.

cacheSolve <- function(x, ...){
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    else
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## These lines will help you test the code in one go. 
## Just remove the "##" marking the code below here as comments and run this whole page.

## a <- matrix(rnorm(1:16),2,2)
## b <- makeCacheMatrix(a)
## cacheSolve(b)
## cacheSolve(b)