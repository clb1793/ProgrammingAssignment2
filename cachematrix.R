## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function will allow an object to cache its inverse. 
## This is important as rather than computing a long or intensive vector or matrix repeatedly, the user can simply use a cached option to work more quickly and efficiently
## Through the cacheSolve function, the inverse of the above matrix will be computed

makeVector <- function(x = numeric()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
    
  }
  get <- function()x
  setmean <- function(mean) c <<- mean
  getmean <- function() c
  list(set= set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  c <- x$getmean()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- mean(data, ...)
  x$setmean(c)
  c
}

## Write a short comment describing this function
## The makeCacheMatrix function creates a matrix that aids in setting the various values and obtaining the 
## values of the matrix, while also doing the same for the inverse 


makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) c <<- inverse
  getinverse <- function() c
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function now works to compute the inverse of the function generated above
## With the function cached, this function should easily obtain it and generate the inverse

cacheSolve <- function(x, ...) {
  c <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinverse(c)
}

## Return a matrix that is the inverse of 'x'

c <- rbind(c(0,2,4),c(2,2,2), c(-8,4,0))
b <- makeCacheMatrix(c)
identical(b$get(), c)

b$getinverse()

cacheSolve(b)

b$getinverse()

View(c)
dim(c)
dim(b)

b$getinverse()

b$set(rbind(sqrt(c(2,2)), c(-1,2)))
b$getinverse()
b$getinverse()

solve(c)
solve(b$getinverse)
