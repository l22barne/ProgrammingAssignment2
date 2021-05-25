makeCacheMatrix <- function(x = matrix()) {
        ## x assumed as (square) invertible matrix
        ## returns a list containing functions to 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse
        ## which is used as input for CacheSolve
        
        inv = NULL
        set = function(y) {
                # <<- used to assign a values in an environment other than the current. function analogue to vector example in assignment
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        
        # returning the list
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # check if the inverse has already been calculated, if so (inv 'not.NULL') get cached data
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # if not invers is unavailable yet, calculating the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache.
        x$setinv(inv)
        
        return(inv)
}
