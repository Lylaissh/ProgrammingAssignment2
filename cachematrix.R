## Funtions to generate inverse of given matrix 
## to understand lexical scoping


## makeCacheMatrix() takes a matrix and returns set of functions
## to set new value to the matrix, get current value of matrix,
## cache computed value of inverse of matrix and get computed value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL             ## Assign initial value to variable "inverse"
  
  
    set <- function(y) {        ## Define funtion "set" to 
      x <<- y                   ## pass new value of matrix. 
      inverse <<- NULL          ## <<- Operator used to allow access to variable
    }                           ## in function call environment. inverse value is assigned 
                                ## Null value, since matrix value is changed to a new one here
                                ## and the previously computed inverse value will thus be incorrect.
  
    get <- function() x                                   ## get value of matrix
    setinverse <- function(solve) inverse <<- solve       ## cache value of computed inverse
    getinverse <- function() inverse                      ## get computed inverted matrix
  
    list(set = set, get = get,                            ## returns value of makeCacheMatrix 
        setinverse = setinverse,                          ## as a list of public functions which can  
        getinverse = getinverse)                          ## then be accessed by $ operator

}


## Return a matrix that is the inverse of 'x'
## cacheSolve() computes inverse of matrix obtained from 
## makeCacheMatrix if not already computed. If computed 
## it returns cached inverse matrix value

cacheSolve <- function(x, ...) {
                    
  inverse <- x$getinverse()         ## Get value of inverse matrix
  
  if(!is.null(inverse)) {           ## Check if value was previously computed
    message("getting cached matrix")
    return(inverse)                 ## If yes, return previously computed inverse value
  }
  
  data <- x$get()                   ## Get value of matirx for which inverse is to be computed
  inverse <- solve(data, ...)       ## Compute inverse of matirx and assign to variable
  x$setinverse(inverse)             ## Cache inverse value of matrix
  inverse                           ## Return the value of inverse matrix
                                    ## either computed or cached
}
