{
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
