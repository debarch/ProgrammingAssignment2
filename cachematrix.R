## cachematrix will return an inverse of a matrix but before it will check
## whether the inverse is already available in cache, if yes
##it will return the same else calculate the inverse

## This creates the cache where to check for existing return and how to return

makeCacheMatrix <- function(x = matrix(data=NA)) {
       
      i <- matrix(data=NA)
      set <- function(y = matrix(data=NA)){
        x <<- y
        i <<- matrix(data=NA)
      }
      
      get <- function() x
      setinv <- function(inv = matrix(data=NA)) i <<- inv
      getinv <- function() i
      
      matrix(c(set = set,get = get, setinv = setinv, getinv = getinv), nrow=1,ncol = 4,dimnames = list(c("1"),c("set","get","setinv","getinv")))
}    

## Calculate the inverse of a given matrix using solve() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #X <- makeCacheMatrix(x)
  mtr <- x[1,]$get()
  
  
  i <- x[1,]$getinv()
  if(!all(is.na(i))){
    message("getting cached data")
    return(i)
  }
  
  data <- x[1,]$get()
  i <- solve(data)
  x[1,]$set(mtr)
  x[1,]$setinv(i)
  i
}
