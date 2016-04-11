## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  set <- function(y)  ##Create a function that lets you input a new matrix
  {
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x ##Create a function that outputs the stored matrix
  
  setinverse <- function(inverse) m <<- inverse ##Create a function that stores the inverse
  
  getinverse <- function() m ##Create a function that outputs the stored inverse
  
  list(set = set, get = get,
       
       setinverse = setinverse,
       
       getinverse = getinverse) ##Create a list as the final argument that contains all 4 functions above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'

    
    m <- x$getinverse()
    
    if(!is.null(m)) {
      
      message("getting cached data") ##If the inverse is already cached, say so
      
      return(m) ##return the cached inverse
      
    }
    
    data <- x$get() ##store the output matrix from the makeCacheMatrix function
    
    m <- solve(data, ...) ##store the inverse of data in a variable m
    
    x$setinverse(m) ##store the setinverse field in x
    
    m ##print the inverse
    
}
