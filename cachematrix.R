## Put comments here that give an overall description of what your
## functions do 
## These two functions take a matrix and calculate its inverse, it is a practice for lexical scopping.
## the second function will return to a cached value of inverse if it was calculated before.

## Write a short comment describing this function
## This function creates a special matrix for the following calculation of its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set,get=get,
            setinverse=setinverse,
            getinverse=getinverse)

}

## Write a short comment describing this function
## This function calculates the inverse of the special matrix created with the function above.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cached and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

