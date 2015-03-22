## Functions that demonstrates the lexical scoping rules of R
## and how they are used for implementing caching functionality
## Functions specifically calculates the inverse of a matrix if
## not done already and caches the same.

## This function creates a vector with list of following functions
## set -> set the value of input matrix
## get -> get the value of input matrix
## setInverse -> set inverse in a variable (cached)
## getInverse -> return inversed matrix that is in cache (null or populated)
makeCacheMatrix <- function(x = matrix()) {
  invcachematrix <- NULL #initialize
  
  set <- function(inmatrix) { 
    x <<- inmatrix ## use deep assignment arrow (<<-) to set x which is in a different environment
    invcachematrix <- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    invcachematrix <<- inverse ## use deep assignment arrow (<<-) to set invcachematrix
  }
  
  getInverse <- function() {
    invcachematrix
  }
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## Get the inverse of given matrix from cache. If it is null calculate and set 
## inverse in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from the special matrix
    inverse <- x$getInverse()
    
    ##if value is not null i.e., inverse was previously calculated return
    if(!is.null(inverse)) { 
      print("Returning cached inverse")
      return(inverse)
    }
    
    ## if there is no inverse value present calculate the same using solve
    inverse <- solve(x$get())
    
    ## set the calculated value in cache
    x$setInverse(inverse)
    
    return(inverse)
}


