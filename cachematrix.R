## MakeCache Matrix takes the input of the dimension for the square matrix and generates a matrix of random values
## it then generates the inverse of that matrix and returns a list of the set and get functions for the two. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
  set_mat<- function(n){
    n<- readline(prompt="Enter the dimension of the matrix: ")
    n<- as.integer(n)
    set.seed(50)
    x<- matrix(rnorm(100),n,n)
    
  }
  get_mat<- function() mat
  
  set_inv<- function(){ 
    inv<-solve(x)
  }
  
  get_inv <- function() inv
  list(set=set_mat, get=get_mat, set_inv= set_inv, get_inv= get_inv)
  
}


## cacheSolve calls the inverse of the matrix from the cache memory if present 
##else calls the set_inv function to generate a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$get_mat()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$set_mat(m)
  return(m)
  
}
