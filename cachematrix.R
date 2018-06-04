## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # begins by setting the inverse to NULL as a placeholder for a future value
        set <- function(y){
                x <<- y
                inv <<- NULL
        } # defines a function to set the vector, x, to a new vector, y, and resets the inverse, inv, to NULL
        get <- function() x # returns the vector x
        setinv <- function(solve) inv <<- solve # take de inverse of x
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) # returns the 'special vector' containing all of the functions just defined
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) { # If the inverse was already calculated, just retrieve it from cache
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) # Calculate the inverse of the matrix
        x$setinv(inv)
        inv
}

r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)

x <- makeCacheMatrix(mat1)
cacheSolve(x)
