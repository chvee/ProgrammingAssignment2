## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL ##initiate a null variable to store the inverse when needed
    ##input.matrix <- x ## Taking the matrix sent and putting into this variable
    
    ## Set subfunction replaces the stored matrix. When set is called we are explicitly
    ## replacing the prior matrix and resetting m to NULL so that cacheSolve will return 
    ## the inverse of the new matrix versus the cached version of the prior one
    ## We must use the <<- notation as input.matrix and cached.inverse are in the parent
    ## environment.
    
    Set <- function(y) {
        x <<- y
        cached.inverse <<- NULL
    }
    
    ## Get subfunction must return the stored matrix. Matrix is stored in input.matrix
    ## Subfunction runs a quick check to verify that a matrix was previously set
    
    Get <- function() {
        if (is.null(x)) {
            message("No matrix was available for return")
        } else {
        return(x)
        }
    }
        
    ## SetInverse is similar to the Set subfunction, but will be used to specifically 
    ## store the inverse matrix. This subfunction only needs to store it in the cached.inverse
    ## variable. We must use <<- as cached.inverse is in the parent environement
   
    SetInverse <- function(inverse.matrix) {
        cached.inverse <<- inverse.matrix
    }
    
    ## GetInverse is a simple function to return the inverse matrix. 
    
    GetInverse <- function() {
        return(cached.inverse)
    }

    ## Similar to object oriented programming to classes, this list effectively
    ## makes the subfunctions defined as public. This way each subfunction can be
    ## called directly from elsewhere. The "public" name of the function can differ
    ## from the internal name that we gave it. 
    
    list(Set=Set, Get=Get, SetInverse=SetInverse, GetInverse=GetInverse)
    
}


## cacheSolve will take a matrix on input. If this is a new matrix it will calculate the 
## inverse and store it. On future calls it will return the cached version of the matrix
## If you want to replace the matrix you can call the set function in makecacheMatrix to replace it

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse.matrix <- x$GetInverse()

    ## If a matrix was previously cached inverse.matrix should not be null. If X never previously existed
    ## the inverse would be null as well
    if(!is.null(inverse.matrix)) {
        message("retrieving cached data")
        return(inverse.matrix)
    }
    
    ## otherwise inverse.matrix is null
    ## we can call x$get because it was created as a matrix with makeCacheMatrix in the parent environment
    ## using x <- makeCacheMatrix(some.matrix). Solve for the inverse and store it back
    ## using the SetInverse subfunction from the makeCacheMatrix function.
    ## we are assuming that this is an invertible matrix. Otherwise we would need to add additional
    ## error handling. i.e. If the matrix is not square or it is a singular matrix
    
    matrix <- x$Get()
    inverse.matrix <- solve(matrix)
    x$SetInverse(inverse.matrix)
    inverse.matrix
    
        
}
