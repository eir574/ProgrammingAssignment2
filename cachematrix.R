## Since computing a matrix inverse can be computationally expensive,
## the combination of the two funtions makeCacheMatrix is designed
## to cache the computation and compute the inverse only when necessary.
## The function makeCacheMatrix essentially wraps the input matrix in
## an environment that contains a cached copy of its matrix (which is not
## computed until the first time it is needed). The function cacheSolve
## then wraps R's solve function such that it is called on the matrix
## only if the inverse has not previously been computed.


## makeCacheMatrix takes a matrix as an argument
## and returns a list of four functions:
##  set: sets the value of the matrix
##  get: returns the value of the matrix
##  setinverse: Sets and cached value of the inverse
##  getinverse: Returns the inverse of the matrix. If it is not already cached,
##              computes and stores the inverse
makeCacheMatrix <- function(x = matrix()) {
  #initialize the matrix inverse to NULL
  m_inverse <- NULL
  
  set <- function(y) {
    x <<- y #set the value of the matrix x in the enclosing environemnt to y  
    m_inverse <<- NULL #since the value of x has changed, remove any previously cached value for its inverse
  }
  
  get <- function() x #simply returns the matrix x
  
  #setinverse takes the matrice's inverse and stores it in m_inverse 
  #in the enclosing environment
  setinverse <- function(inv) m_inverse <<- inv
  
  getinverse <- function() m_inverse #simply returns the inverse of the matrix
  
  #produce the list of functions that is returned by makeCacheMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve computes the inverse of the matrix represented by x
## The input value x must be the return value of makeCacheMatrix
## The inverse will only be computed if it is not already cached  

cacheSolve <- function(x, ...) {
    #Get the cached value of the inverse. Will be
    #NULL if the inverse has not already been cached
    inv <- x$getinverse()
    
    #If inv!=NULL, then the inverse has already been calculated, and
    #we can return the cached value
    if(!is.null(inv)) {
      print("returning cached value")
      return(inv)
    }
    
    data <- x$get() #get the matrix

    inv <- solve(data,...) #compute the inverse
    
    x$setinverse(inv)  #store the inverse
    
    inv #finally, return inv
    
}
