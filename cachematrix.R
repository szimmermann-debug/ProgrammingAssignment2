
## creating a function that return a own maxtrix like class that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## when the object is created, the inverse isn't calculated, therefore i is NULL
  ## but the matrix x is created
  i <- NULL
  
  ## define the set- and get functions to allow that x (the matrix) and i (its inverse) could be accessed for reading and writing
  
  get <- function() x
  getinverse <- function() i
  
  set <- function(y){
    x <<- y
    
  }
  
  setinverse <- function(inverse){
    i <<- inverse
  }
  
  ## return the class as list of functions
  return(list(get = get, getinverse = getinverse, set = set, setinverse = setinverse))
  
}



# this function works with an object that is created by the function makeCacheMatrix()
# the function checks if the inverse is already calculated. If yes, the inverse is returned. 
# If not, the inverse is calculated and stored within the object by using the setinverse() method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)){
          message("getting cached data")
          return(i)
        }
        else {
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          return(i)
        }
}


### example with standard matrix

matrix_example1 <- matrix(c(1,2,2,2,4,1,0,1,0), 3, 3)
solve(matrix_example1)

### example with own class of matrix
matrix_example2 <- makeCacheMatrix(matrix_example1)
matrix_example2$get()

matrix_example2$getinverse() #inverse isn't calculated yet
cacheSolve(matrix_example2)  #inverse is calculated
matrix_example2$getinverse() # the cached inverse is retrieved
