## Write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) { # 1st function and initialize the 
                                            # matrix x
        m <- NULL # initialize the object m
        set <- function(y) { # 2nd function, nested. Set values for x and m.
                x <<- y # setting value of x in parent environment
                m <<- NULL # setting value of m in parent environment
        }
        get <- function() x # 3rd function nested in 1st. Get or access x. Uses 
                            # uses lexical scoping to get from correct or
                            # parent environment
        setinverse <- function(inverse) m <<- inverse # 4th function nested in 1st
                                                      # defines the setter for 
                                                      # the inverse m in the 
                                                      # parent environment
        getinverse <- function() m # 5th function nested in 1st. Get inverse m
                                   # uses lexical scoping to get from parent
                                   # environment
        list(set = set, get = get, # creates a new object by returning a list 
             setinverse = setinverse, # with each element named and subsettable.
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache. 

cacheSolve <- function(x, ...) { # start function with a single argument and 
                                  #ellipsis that allows to pass additional arguments.
        m <- x$getinverse() #attempts to retrieve the inverse.
        if(!is.null(m)) { #if result is not equal to null, then we have a 
                message("getting cached data") # valid result to return to parent
                return(m) # 
        } # if result is null then we calculate below
        data <- x$get() # Get the matrix from input
        m <- solve(data, ...) #calculate the inverse from above
        x$setinverse(m) # set inverse in x
        m # return inverse to parent environment
}
