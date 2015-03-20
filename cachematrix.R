## Invert a matrix and cache its value for faster load times

## Define a list of functions to be used in cacheSolve function
makeCacheMatrix <- function(input_mat = matrix()) {
  cached_mat <- NULL
  
  ## Reset cached_mat to NULL and update cached value of input_mat
  reset <- function(y) {
    input_mat <<- y
    cached_mat <<- NULL
  }
  
  ## Return inputted matrix
  getinput <- function() {
    return (input_mat)
  }
  
  ## Update cached value of inverted matrix
  setmat <- function(inverted) {
    cached_mat <<- inverted
  }
  
  ## Return cached matrix
  getmat <- function() {
    return(cached_mat)
  }
  
  ## Return functions as a list
  list(reset = reset, getinput = getinput, setmat = setmat, getmat = getmat)
}


## Return inverted matrix based on previously defined functions
cacheSolve <- function(func_list, ...) {
  
  # Load cached value of inverted matrix
  inv_mat <- func_list$getmat()
  
  # Return cached value if it exists
  if(!is.null(inv_mat)) {
    message("Getting cached matrix")
    return(inv_mat)
  }
  
  # If not, invert and output matrix passed to makeCacheMatrix() and update cached value
  else {
    input_mat <- func_list$getinput()
    inv_mat <- solve(input_mat, ...)
    
    func_list$setmat(inv_mat)
    return(inv_mat)
  }
}