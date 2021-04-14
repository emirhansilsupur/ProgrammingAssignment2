makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) k <<- solve
  getsolve <- function() k
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  k <- x$getsolve()
  if(!is.null(k)) {
    message("getting inversed matrix")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setsolve(k)
  k
}
n <- matrix(rnorm(9),3,3)
n1<- makeCacheMatrix(n)
cacheSolve(n1)
