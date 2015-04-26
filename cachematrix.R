## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        
        # Default Cache is Null
        Cache <- NULL
        
        # Create Matrix
        setMatrix <- function(values) {
                x <<- values
                # since the matrix is assigned a new value, flush the cache
                Cache <<- NULL
        }

        # Returns Matrix Values
        getMatrix <- function() {
                x
        }

        # Cache function
        cacheInv <- function(solve) {
                Cache <<- solve
        }

        # Cached Value
        getInv <- function() {
                Cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInv = cacheInv, getInv = getInv)
}



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(y, ...) {
        # Get Cached Value
        inv <- y$getInv()
        # Return Cached Value if not null
        if(!is.null(inv)) {
                return(inv)
        }
        # Caclulate inverse
        data <- y$getMatrix()
        inv <- solve(data)
        y$cacheInv(inv)
        
        # Print Inverse
        inv
}
