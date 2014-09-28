makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Cache Program 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached data retrieved")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

Output:

> x=rbind(c(1,2),c(3,4))
> m=makeCacheMatrix(x)
> cacheSolve(m)
[,1] [,2]
[1,] -2.0  1.0
[2,]  1.5 -0.5
> x=rbind(c(1,0,1),c(1,4,5),c(1,2,5))
> m=makeCacheMatrix(x)
> cacheSolve(m)
[,1]  [,2] [,3]
[1,]  1.25  0.25 -0.5
[2,]  0.00  0.50 -0.5
[3,] -0.25 -0.25  0.5
