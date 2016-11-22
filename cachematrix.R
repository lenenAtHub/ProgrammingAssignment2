
## makeCacheMatrix is a function that stores several other functions in a list, in this case set, get, setinv, getinv
# when passing a matrix X as an argument, makeCacheMatrix will create an object (e.g. a in case a<-makeCacheMatrix(X))
#that contains the list of the four functions, the object inv is still null
# in a 2nd step the object a is passed to cacheSolve and an object b is created: b<-cacheSolve(a)
# cacheSolve calculates the inverse of the matrix X assigned to a, so b = inverse of X
# at the same time the inverse of X is stored in a! test: q<-a$getinv(), View(q)


makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(Y){
          X <<- Y
          inv <<- NULL
    }
    get <- function() X
    setinv <- function(inversemat) inv <<- inversemat
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
  inv <- X$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data,...)
  X$setinv(inv)
  inv
}
