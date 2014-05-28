## makeCacheMatrix() and cacheSolve()
## return cached inverse matrix for invertible square matrix
## calculate the inverse matrix if no cached inverse matrix
## E.g., x<-makeCacheMatrix(matrix(c(1,0,1,5,-3,1,2,4,7),nrow=3))
##        cacheSolve(x)


# create cache for inverse matrix
# Argument must be an invertible matrix
# The argument should include square matrix data and number of row
# E.g., matrix(c(1,0,1,5,-3,1,2,4,7),nrow=3)
## makeCacheMatrix() there are API for 4 functions register to a list 
## 1.get square invertible matrix
## 2.set square invertible matrix from user
## 3.get cached inverse matrix    
## 4.set inversed matrix 

makeCacheMatrix <- function(x = matrix()) {
    # clear the cached inversed matrix
    inv <- NULL
    # get()  return matrix 'x' data
    get<-function(){
        
        x
    }
    
    ##set() setting an matrix and clear the cached inverse matrix
    
    set <- function(z) {
        x <<- z
        inv <<- NULL
    }
    ## setInverseMatrix() setting cache for inverse matrix
    
    setInverseMatrix<-function(w){
        
        inv <<- w
        
    }
    ## getInverseMatrix() return an iverse matrix
    getInverseMatrix<-function() {
        
        inv
    }
    ## API interface for 
    list(get=get, set=set,
         getInverseMatrix = getInverseMatrix,
         setInverseMatrix = setInverseMatrix)
    
    

}


## cacheSolve() to comput the inverse matrix if there is 
## no cached inversed matrix
## argument is the return of the makeCacheMatrix()

cacheSolve <- function(x, ...) {
   ## get the cached inverse matrix from makeCacheMatrix()
    i <- x$getInverseMatrix() 
    
    ## return the cached data if if the inverse Matrix exists
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## compute the inverse matrix
    ## get matrix data
    data <- x$get()
    ## compute the inverse matrix by solve()
    ans <- solve(data)
    
    ## set inverse matrix to cache in makeCacheMatrix()
    ## and display if the inverse matrix not NULL
    if (!is.null(ans)) {
        x$setInverseMatrix(ans)
        ans
    }
    else 
        message("inverse matrix does not exist!!")
    
    
}
