## makeCacheMatrix() is a function for setting up and catching the inverse of a matrix; 
## function sets the value of "m" to NULL and then uses "m" as a placeholder 
## for the catched inverse calculation via solve() in the function cachesolve()

## set() function allows to change the elements in the matrix, e.g. x$set(matrix(...)) 
## in case a new matrix is created "m" is reset to NULL

## get() function shows the elements in the matrix
## getinverse() function shows the cached inverse, if no inverse is cached value is NULL

## setinverse() function alows to set the value of "m" from the result in cachesolve(), 
## this is the value retrived if no changes have been made to the matrix

makeCacheMatrix  <- function(x = matrix()) {    
      m  <- NULL                                                  
      set  <- function(y) {                     
            x <<- y 
            m <<- NULL
      }     
      get  <- function() {x}
      setinverse  <- function(inverse) {m  <- inverse} 
      getinverse <- function() {m}
      list(set = set,                           
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cachesolve() calculates the inverse of a matrix by taking the value of "m" 
## from makeCacheMatrix(); it uses an if statement that evaluates "m"; if "m" = NULL 
## an inverse for the given matrix is calculated and saved as the new value of "m"; 
## if m =/= NULL the current value of m is printed with the comment "getting cached data"

## the caching is achieved via the manipulation of the variable "m", when a matrix 
## is created or edited "m" = NULL, that prompts the use of solve() in the cachesolve() 
## function that saves its output in "m", as long as m =/= NULL the inverse is retrived 
## by printing "m"

cachesolve  <- function(x, ...) {               
      m  <- x$getinverse()                      
      if(!is.null(m)) {                        
            message("getting cached data")      
            return(m)
      }
      data  <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)                           
      m                                         
}
