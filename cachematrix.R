## https://github.com/Moosebaby/ProgrammingAssignment2
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a list to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(
  set = set,
  get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the "matrix" 
## created with makeCacheMatrix function.
## Firstly, it checks to see whether the inverse has been calculated already
## If that is the case,it'll get the inverse from the cache instead of 
## computating again.
## Otherwise, the function will calculate the inverse of the matrix and 
## set the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
