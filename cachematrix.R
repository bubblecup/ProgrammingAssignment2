## Functions to create a special matrix that can cache its inverse,
## in fulfillment of Coursera Intro to R assignment 2

## Constructor function constructs a special matrix that can cache its invert 
## Arg `x` is assumed to be an invertible matrix
## Returns a list of get/set methods that can be performed on the matrix
makeCacheMatrix <- function(x = matrix()) 
{
        # Invert of x, intialized to NULL
        invert<-NULL # note check if this is necessary or can initialize using set
        
        # Set the value of the matrix and initialize invert to NULL
        set<-function(y) 
        {
                x <<- y
                invert <<- NULL
        }
        
        # Retrieves the matrix
        get<-function() x
        
        # Sets the invert given its value
        setinvert<-function(inv) invert<<-inv
        
        # Retrieves the invert
        getinvert<-function() invert
        
        # Return a list consisting of get/set methods that can be performed on the matrix
        list(set=set, get=get, setinvert=setinvert, getinvert=getinvert)
}

## The following function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated and cached, it returns the cached value
## Arg `x` is the matrix returned by makeCacheMatrix, ... are additional args to be passed to `solve` 
cacheSolve <- function(x, ...) 
{
        # Retrieve the invert
        inv<-x$getinvert()

        # Compute if null, and cache and return computed value
        if (is.null(inv))
        {
                inv<-solve(x$get(),...)
                x$setinvert(inv)
        }
        # If cached value is present, return it along with a message to indicate so
        else
        {
                message("getting cached data")
        }
        inv
}