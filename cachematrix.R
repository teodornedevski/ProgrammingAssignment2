## The goal of these fucntions is to save compute time on calculations taht 
## require calculating the inverse of a matrix
## cacheSolve calls the makeCacheMatrix functions to check if there is already
## a stored inverse for that matrix. if there is not one is calculated and returned

## makeCacheMatrix takes a matrix and has the capability to store its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #setting the default value of the matrix to NULL
  ## function that clears the inverted matrix and stores the new initial matrix
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x #function that returns the value of the matrix
  setInverse <- function(matrix) inverse <<- matrix # function that sets the inverted matrix
  getInverse <- function() inverse # function that returns the inverted matrix
  
  ## return the functions as a list that can be interfaces
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## returns the inverse of a matrix x, while making sure that it uses the cached 
## inverse if it is already calculated

cacheSolve <- function(x, ...) {
  #check if the inverse exits and return it if yes
  inverse <- x$getInverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  # if the inverse does not exist get the matrix
  data <- x$get()
  #solve the matrix
  inverse <- solve(data)
  #store the inverse
  x$setInverse(inverse)
  inverse ## Return a matrix that is the inverse of 'x'
}

