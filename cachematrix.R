## So this function creates a list of functions to go along with the matrix which is passed into its namespace.
## The namespace also contains a local variable, x_inv to store its inverse.
## The functions will set the matrix, get the matrix, set the inverse & get the inverse.


makeCacheMatrix <- function (x = matrix()) {
        
        
        ##  Initializing x_inv
        
        x_inv <- NULL
        
        
        ##  Our first child function will "set" x & reinitialize x_inv and then updates them into the
        ##      namespace of the parent function.
        
        set <- function(y) {
                
                x <<- y
                x_inv <<- NULL
        }
        
        ##  Now that we've created (and potentially updated) x & x_inv, we can create our last 3 functions.
        
        ##  This function "gets" the original matrix out of the namespace of makeCacheMatrix.
        
        get <- function() x
        
        ##  This function can be called to update x_inv in the namespace pf makeCacheMatrix.
        
        setinv <- function(inverse) x_inv <<- inverse
        
        ##  This function "gets" the inverse of x out of the namespace of makeCacheMatrix.
        
        getinv <- function() x_inv
        
        ##  And, lastly we'll compile these functions into a list of functions and returns it.
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##  This function inverts a matrix that has been stored in the namespace of x as the output of 
##      the makeCacheMatrix function.
##  If the matrix has already been inverted, this function will simply retrieve the cached inverse
##      from the namespace of makeCacheMatrix rather than inverting it again.


cacheSolve <- function (x, ...) {
        
        ##  first we "get" the value stored in x_inv from x(the argument we passed cacheSolve) and store it here.        
        
        x_inv <- x$getinv()
        
        ##  if the inverse is already stored in x, we don't want to calculate it again so we just pull it from cache.
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        
        
        ##  otherwise, we get the matrix...
        data <- x$get()
        
        ##  and then we invert it...
        x_inv <- solve(data, ...)
        
        ##  and then we cache it so we can pull it next time without having to calculate it.
        x$setinv(x_inv)
        
        ##  then we return the inverse.
        x_inv
}