## The function cacheMatrix creates a list of 4 functions to:
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the matrix-inverse.
## 4. get the value of the matrix-inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the matrix-inverse
        inv <- NULL
        
        ## Definition of the method that sets the matrix.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Definition of the method that gets the matrix.
        get <- function() {
                ## Return the matrix
                x
        }
        
        ## Definition of the method that sets the matrix-inverse.
        setInverse <- function(inverse) {
                inv <<- inverse
        }
                
        
        ## Definition of the method that gets the matrix-inverse.
        getInverse <- function() {
                ## Return the matrix-inverse
                inv
        }
        
        ## return a list of the 4 previously declared functions
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        
}


## cacheSolve computes the inverse of the matrix returned by the cacheMatrix
## function described above. The matrix-inverse is only calculated if its value
## is not already cached.

cacheSolve <- function(x, ...) {
        
        ## Return the matrix-inverse of x
        inv <- x$getInverse()
        
        ## Return the matrix-inverse if already set(=cached).
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Store the matrix from our object in "data".
        data <- x$get()
        
        ## Calculate the matrix-inverse of our matrix.
        inv <- solve(data)
        
        ## Set the matrix-inverse.
        x$setInverse(inv)
        
        ## Return a matrix-inverse.
        inv
}