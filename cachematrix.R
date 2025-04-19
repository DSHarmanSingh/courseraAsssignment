makeCacheMatrix <- function(x= matrix()){
  inv<- NULL#This will store the cache inverse
  set<- function(y){
    x<<- y  #Set new matrix
    inv<<- NULL#Clear cached inverse
  }
  get<- function() x
  
  setInverse<- function(inverse) inv <<- inverse
  getInverse<- function() inv
  
  list(set= set,
       get= get, 
       setInverse= setInverse, 
       getInverse= getInverse)
}

cacheSolve<- function(x, ...){
  inv<- x$getInverse()
  
  if (!is.null(inv)){
    message("Getting cached inverse.")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)#Compute inverse
  x$setInverse(inv) #Cache it
  inv
}

#Example usage
m <- matrix(c(2,1,1,2), 2,2)

#Create a special object
cacheMatrix<- makeCacheMatrix(m)

#First call: computes inverse
cacheSolve(cacheMatrix)

#Second call: uses cached inverse
cacheSolve(cacheMatrix)
