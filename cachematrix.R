## Below are two functions that are used to create a special object that stores inverse matrix
## and caches it. You would test this by inputing the following in the R Console:
# > source('../ProgrammingAssignment2/cachematrix.R') # You would first source this file
# > xm <- matrix(1:4,2,2) # Creates a 2 x 2 matrix
# > cache_matrix_object <- makeCacheMatrix(xm) # Passes matrix and gets special matrix object
# > cacheSolve(cache_matrix_object) # Solves inverse and stores it in 'special' 'cache' object
# > cacheSolve(cache_matrix_object) # Returns a cached inverse

## This is a function that creates a 'special' object made of list of functions that are invoked
## from different enviroment and are belonging to this enviroment. They're used to set 'msolve' 
## in runtime of that other enviroment that is calling 'setmsolve()'. 
## What this function returns is a list of functions that are available in invoking enviroment. 
## This enviroment will be available to that other running enviroment because this 'special' 
## object will be passed as a parameter to the running enviroment and that's how we read or 
## assign a value to the object in this enviroment.

makeCacheMatrix <- function(x = matrix()) {
        
        # Let's default to NULL so we can test if we get something in cacheSolve
        msolve <- NULL 
        
        get <- function() { x }       
        setmsolve <- function(cachemsolve) { msolve <<- cachemsolve }
        getmsolve <- function() { msolve }
        
        list(get = get,
             setmsolve = setmsolve,
             getmsolve = getmsolve)

}

## The following function calculates inverse matrix created with the solve() function. 
## However, it first checks to see if the inverse matrix has already been solved. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calls solve(x$get()) with matrix as a parameter and saves inverse matrix
## into cache by calling x$setmsolve(msolve) before returning msolve which is inverse matrix.

cacheSolve <- function(x, ...) {
        
        # Getting the cached matrix, hopefully not NULL
        msolve <- x$getmsolve() 
        
        # If not NULL then we got msolve and just return msolve .. done
        if(!is.null(msolve)) { 
                message("getting cached data")
                return(msolve)
        }
        
        # Lets' get matrix vector x from makeCacheMatrix function 'get'
        message("calculate inverse matrix")
        msolve <- solve(x$get()) 
        
        # Let's set msolve in MakeCacheMatrix i.e. pass it as a 'cachemsolve' (look)
        # MakeCacheMatrix for the setmsolve
        x$setmsolve(msolve) 
                
        ## Return a matrix that is the inverse of 'x'        
        msolve
}