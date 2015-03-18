## Hello peer evaluator!

## Thank you for taking the time to look through my work.

## ---------------------------------------------------------------------------

## makeCacheMatrix and cacheSolve reduce processing time by caching the result
## of the matrix inversion (a complex problem) so that it may be retrieved later
## insead of re-calculated

## ---------------------------------------------------------------------------

## My makeCacheMatrix creates a special "matrix" (ie list of functions) which
## will enable the caching process

makeCacheMatrix <- function(x = matrix()) {

                m <- NULL               ## freeing up m in the local env
                
                set <- function(y) {    ## sets the normal matrix
                        x <<- y
                        m <<- NULL
                }
                
                get <- function() { x } ## gets the normal matrix
                
                setinv <- function(matrix) { m <<- matrix }     ## sets inverse
                
                getinv <- function() { m }                      ## gets inverse
                
                list(set = set, get = get,      ## write the list of functions
                     setinv = setinv,
                     getinv = getinv)
        }
   
## ----------------------------------------------------------------------------

## cacheSolve determines whether the matix inverse has been cached, and then
## it will either return the value or compute the inverse and then cache it

cacheSolve <- function(x) {
        
        m <- x$getinv()         ## get inverse from the cache location
        
        if(!is.null(m)) {       ## if the cached inverse exists...
        
                message("getting cached data")
                return(m)       ## then we'll return it and quit
        
        }
        
        matrix <- x$get()       ## if the inverse didn't exist, we'll get the
                                ## original matrix...
        
        m <- solve(matrix)      ## solve it
        
        x$setinv(m)             ## and cache the solution
        
        return(m)               ## and finally return the inverse
}