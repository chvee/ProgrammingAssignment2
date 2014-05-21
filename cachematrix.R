## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL ##initiate a null variable to store the inverse when needed
    input.matrix <-x ## Taking the matrix sent and putting into this variable
    
    ## Set subfunction replaces the stored matrix. When set is called we are explicitly
    ## replacing the prior matrix and resetting m to NULL so that cacheSolve will return 
    ## the inverse of the new matrix versus the cached version of the prior one
    ## We must use the <<- notation as input.matrix and cached.inverse are in the parent
    ## environment.
    
    Set <- function(user.matrix) {
        input.matrix <<- user.matrix
        cached.inverse <<- NULL
    }
    
    ## Get subfunction must return the stored matrix. Matrix is stored in input.matrix
    ## Subfunction runs a quick check to verify that a matrix was previously set
    
    Get <- function() {
        if (is.null(input.matrix)) {
            message("No matrix was available for return")
        } else {
        return(input.matrix)
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
    
    list(set=set, get=get, SetInverse=SetInverse, GetInverse=GetInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
