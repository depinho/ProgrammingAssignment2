## This function creates a special "matrix" object, 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) 
{
    InvMat <- NULL ##define inverted matrix variable
    
    set <- function(y) ##set the value of the matrix
    { 
        x <<- y            
        InvMat <<- NULL
    }
    
    get <- function() x ##get the value of the matrix
    
    setinv <- function(inv) ##set the value of the inverted matrix
    {
        InvMat <<- inv    
    }
    
    getinv <- function() InvMat ##get the value of the inverted matrix
        
    #Return function list
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) ## Return a matrix that is the inverse of 'x'
{
    InvMat <- x$getinv()
    if(!is.null(InvMat)) {
        message("getting cached matrix")
        return(InvMat)
    }
    ## if inverted matrix has not been calculated/cached then...
    data <- x$get()
    InvMat <- solve(data, ...)
    x$setinv(InvMat)
    InvMat ##return inverted matrix
}
