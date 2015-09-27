## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" objet that can cache 
## its inverse. It creates a list that sets the value of the matrix, gets the 
## value of the matrix, sets the value of the inverse of the matrix, and gets 
## the value of the inverse of the matrix.

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function returns the inverse of the matrix. It assumes that 
## the matrix is always invertible. It first checks if the inverse has already
## been computed. If so, it returns the message "getting cached data" and then 
## displays the inverse of the matrix, skipping the computation. If it has not 
## previously been calculated it computes the inverse of the matrix by first 
## setting the value in the cache with the setinverse function and then 
## displaying the inverse.

cacheSolve <- function (x, ...){
        m <- x$getinverse()
        if(!is.null(m)){
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
