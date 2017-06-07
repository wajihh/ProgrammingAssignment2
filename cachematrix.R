# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse ofmatrix rather than computing it repeatedly therefore this approach 
# followed in the following pair of functions.

## The following functions have been written using the coding pattern of the two sample 
## functions for "vector" and "mean" given in assignment 2 satement

## Here makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## For this function Input should always be an invertible matirx otherwise error will occur

makeCacheMatrix <- function(x = matrix()) {
 
  invMat  <- NULL
  
  # Set Matrix value
  
  setMat <- function(y){
      
      #<<- operator is used to assign a value to an object in an 
      #environment that is different from the current environment
     
       x  <<- y
  
       invMat <<- NULL
       
  }
  
  # get value of  input matrix
  
    getMat <- function()x
    
# set value of invertible matrix

    setInv <- function(inv) invMat <<- inv

     getInv <- function() invMat

     list(setMat=setMat,getMat=getMat,setInv=setInv,getInv=getInv)
    
}

# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. If yes, the result is obtained and the computation is
# skipped, else , it computes the inverse, sets the value in the cache using setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    invMat <- x$getInv()
        
    ## Check inverse matrix is not NULL then print the message
    
        if(!is.null(invMat)){
            
            message ("getting cached matrix")
       
        return(invMat)

        } 
    
    ## In case value of invertible matrix is NULL then get input matrix data
    
    matdata <- x$getMat()
    
    ## Invoke solve function to find inverse
    
        invMat<-solve(matdata,...)
        
    ## set the invertible matrix
        
        x$setInv(invMat)
    
        ## return value of the inverse matrix
        
        return(invMat)
}

## Follow underlined steps to have your results from above functions:

# First Source both above functions in the Console
# 1. Now generate a square matrices named A, B, C,D etc
# 2. Validate their inverse through R function solve()
# 3. Cache your matrix into above function e.g.
##for matrix A make a new matrix called cacheA <- makeCacheMatrix(A)
# 4. Run cacheA in console it should return the cached matrix A
# 5. Now run function cacheSolve(cacheA), it will return the inverse of cached matrix A
# 6. Run more examples using matrices B, C,D etc of various dimensions using above process
##In case a non invertible matrix (a singular matrix) is cahched an error will result.
