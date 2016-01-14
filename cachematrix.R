## This function super-assigns matrix inverses to the cache using <<-
## This allows the matrix inverse to be calculated only the first time, with the inverse matrix then cached
## For the second (and further) times requested, the inverse will be returned directly from the cache

## This first function will cache the matrix inverses
## It is written to be broadly parallel with makeVector from the Coursera example

makeCacheMatrix <- function(x = matrix()) {
    
    # Assign myInverse as a NULL; had hoped to do this as an empty matrix instead, but NULL worked better
    myInverse <- NULL
    
    # It is not clear that "set" from makeVector was doing anything.  Wish this could be explained ... 
    # I suppose it is nice functionality since I can then use <mytempfile>$set() on the command line
    # The code seemed to run properly without set, but better to over-code than under-code for now
    set <- function(y) {
        x <<- y
        myInverse <<- NULL
    }
    
    # This will pull down the data of interest from the cached vector
    # This also seems unnecessary since x$get() returns x.  Really wish this had been (or could be) explained.
    # Same thoughts about over-code vs. under-code for now
    get <- function() x
    
    # This super-assigns the inverted matrix to myInverse; core component of the code
    setInverse <- function(inverted) myInverse <<- inverted
    
    # This pulls down the super-assignment (cached myInverse) variable; other core component of the code
    getInverse <- function() myInverse
    
    # This creates the vector containing the inverses; I suspect set and get may be redundant
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function will invert a matrix
## It first looks for the inverse in the cache; if that is missing, it inverts the matrix and caches the result
## It is written to be broadly parallel with cachemean from the Coursera example

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check what is in the cached vector for the inverse of x
    myInverse <- x$getInverse()
    
    ## If you have anything other than the default NULL, return the cached result
    if(!is.null(myInverse)) {
        message("getting cached inverse")
        return(myInverse)
    } 
    
    ## There is no need for an else since return has exited the function when if above resolves TRUE
    ## We only get here if no cache.  So, invert the requested matrix and cache the results
    
    ## Still not that clear why we cannot just use myMatrix <- x, though this does test out OK
    myMatrix <- x$get()
    
    ## The instructions indicate the matrix can be inverted (no singularities and the like), so no need to check
    myInverse <- solve(myMatrix, ...)
    
    ## Place the inverted matrix in the special vector so that it is cached
    x$setInverse(myInverse)
    
    ## Return the myInverse (inverted matrix) for this vector
    message("needed to invert this; it was not previously cached but is now")
    myInverse
    
}
