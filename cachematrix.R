##  Put comments here that give an overall description of what your functions do

##  mo<-makeCacheMatrix(m) function take matrix 'm' 
##  create "matrix" object 'mo' to store original and inverse matrix 
##  but does not calculate the inverse matrix

##  cacheSolve(mo) function take "matrix" object 'mo' created 
##  with makeCacheMatrix() function
##  Retrieve and return inverse matrix stored in "matrix" object 'mo'.
##  If nothing is stored yet, cacheSolve(mo) calculates the inverse 
##  of the original matrix, saves it in "matrix" object 'mo'
##  and also return inverse matrix  
##  



makeCacheMatrix <- function(x = numeric()) {

    ##  makeCacheMatrix(x) function take matrix 'x',  
    ##  and create "matrix" object that will store
    ##  original matrix 'x',  
    ##  inverse of matrix 'x',
    ##  and have 4 functions to access (set\get) original 
    ##  and inverse matrix within created 'matrix' object
    ##  !!! no test if the matrix is square
            
    m <- NULL  # create empty variable to store inverse matrix when calculated
        
    # $set(Y) - function to:  
    # - save original matrix (provided as "y") in this "matrix" object
    # in variable "x" in parental environment using "<<-" deep assignment;
    # - because no inverse matrix calculated yet,
    # set m=NULL in parent environment using "<<-" deep assignment
    set <- function(y) {
        x <<- y             
        m <<- NULL    
    }
    
    # $get()  fuction to extract stored original matrix 
    get <- function() x
    
    # $setinv(invmat) - function to save inverse matrix 
    # provided with "invmat" argument
    # use "<<-" deep assignment to store inverse matrix 
    ## in variable 'm' of parental environment 
    # !!! to be accessed only via cacheSolve() function
    setinv <- function(inv) m <<- inv
    
    # $getinv()  function to extract stored inverse matrix
    getinv <- function() m
    
    print(m)
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
    
    ##  cacheSolve(x) function take "matrix" object 'x' created 
    ##  with makeCacheMatrix() function, 
    ##  and retrieves the inverse matrix, stored within the "matrix" object 'x' 
    ##  If inverse matrix is already stored (was calculated before) 
    ##  'cacheSolve' function returns this stored data, and does not calculate inverse matrix anew 
    ##  If inverse matrix is not stored within the "matrix" object 'x' (=NULL)  
    ##  'cacheSolve' function calculates the inverse of the matrix, 
    ##  saves it within "matrix" object 'x'
    ##  and return inverse of the matrix.
    
    
    # get inverse matrix from "matrix" object 'x' 
    m <- x$getinv()
    if(!is.null(m)) {
        # yes, inverse matrix (object 'm') exists and was retrieved from cache 
        # return inverse matrix 'm' and exit this function
        message("getting cached inverse matrix")
        return(m)
    }
    
    # proceed if no inverse matrix is cached in "matrix" object 'x' (m=NULL)
    data <- x$get()         # get original matrix stored in "matrix" object 'x'

    m <- solve(data, ...)   # calculate inverse of the matrix
    
    x$setinv(m)             # put the inverse matrix back into "matrix" object "x" 
    m                       # return inverse matrix
}


