makeCacheMatrix <- function(x = matrix()) {
        ## x: is a square matrix which is invertible
        ## return: a list of functions    1. set the matrix     2. get the matrix     3. set the inverse     4. get the inverse
                  
        inv = NULL
        set = function(y) {
                # `<<-` is used to assign a value to an object in an environment 
          
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix()
        
        inv = x$getinv()
        
        ## Inverse has already been calculated  :
        
        if (!is.null(inv)){

                ## get it from the cached memory

                message("getting cached data")
                return(inv)
        }
        
                ## Calculates the inverse 

        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## sets the value of the inverse in the cache
       
         x$setinv(inv)
        
        inv
}
