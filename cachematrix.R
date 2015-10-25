## Matrix inversion is very costly in terms of time (especially if you deal with big dimensions 
## matrices), so it may be a good idea to check if you have already computed the inverse of a matrix
## and use that result, instead of computing it again. These two functions do exactly this task.

# The function makeCacheMatrix creates a special "vector", which is really a list containing a 
# function to: 
#               1. set the value of the matrix
#               2. get the value of the matrix
#               3. set the value of the inverse matrix
#               4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m.inv <- NULL
        set <- function(y) {
                x <<- y
                m.inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m.inv <<- inverse
        getinverse <- function() m.inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function returns the inverse of the matrix. 
# It does so by checking if the inverse has already been computed. If this is the case, the 
# function does not perform any computation but just copies the result. On the other hand, if the 
# inverse has not been computed yet, the function computes it.
# NOTE: This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m.inv <- x$getinverse()
        if(!is.null(m.inv)) {
                message("getting cached data")
                return(m.inv)
        }
        data <- x$get()
        m.inv <- solve(data)
        x$setinverse(m.inv)
        m.inv
}
