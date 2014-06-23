## stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #initialize variables
  inv <- NULL
  
  
  #create 4 functions:
    #1. set matrix - stores the matrix and wipes out the cache
    setmatrix <- function (y) {
      inv <<- NULL
      x <<- y
    }
  
    #2. get matrix - returns the stored matrix (so you can assign the matrix, plus functions elsewhere) 
    getmatrix <- function() x
  
    #3. set inv    - sets the inverse cache to whatever you tell it
    setinv <- function (inverseval) {
        inv <<- inverseval 
    }
  
    #4. get inv    - just returns the cached inv
    getinv <- function () inv
  
    #wrap the 4 functions up into a list
    list(setmatrix= setmatrix,
         getmatrix= getmatrix,
         setinv   = setinv,
         getinv   = getinv)
  
}



## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
      #test if x$getinv is null
       if(is.null(x$getinv()) ) {
           #if so, get the matrix itself...
           matr <- x$getmatrix()
           # then solve for the inverse
           inv  <- solve(matr)
           
      #if it's not null
      } else {
          #just grab the cached inverse
           inv <- x$getinv()
      }
      
    return(inv)
  
}
