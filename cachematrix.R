## Put comments here that give an overall description of what your
## functions do

## creat a pair of functions that in total can:
## 1. return the inverse of a matrix if it has been cached before
## 2. calculate and cache the inverse of a matrix if it has not been calculated

## Write a short comment describing this function
## return a list of functions that can
## 1. set the matrix value
## 2. return the matrix value
## 3. set the inverse matrix
## 4. return the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL #inverse of x, is NULL if not setted
  ## set the matrix using y, used if need to change the value of the matrix
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  ## return the original matrix
  get <- function() x
  ## if the inverse matrix is known, cache it
  setinver <- function(inver) xi <<- inver
  ## return the inverse of the matrix
  getinver <- function() xi
  list(set = set, get = get, 
       setinver = setinver,
       getinver = getinver)
}


## Write a short comment describing this function
## using the object from function makeCacheMatrix
## 1. return the inverse matrix if it is cached, and print notification
## 2. calculate and cache the inverse of the matrix if it is not cached before
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$getinver()
  if(!is.null(xi)){
    ## cache not empty, using the cached value, print out message
    message("getting cached data")
    return(xi)
  }
  ## cache is empty, the inverse has not been calculated before, calculate and cache
  data <- x$get()
  xi <- solve(data)
  ## cache the inverse
  x$setinver(xi)
  xi
}

## TEST EXAMPLE AND RESULT
## the first call of cacheSolve, the inverse has 
## not been calculate, so it is calculated and cached
## the second call of cacheSolve, the cached value is returned
## x <- 1:4
## dim(x) <- c(2,2)
## y <- makeCacheMatrix(x)
## cacheSolve(y)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(y)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


