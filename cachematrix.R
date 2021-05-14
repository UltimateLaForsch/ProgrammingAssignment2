## R function that is able to cache potentially time-consuming inverse matrix computations


# Creating a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set=set, get=get, setInverse=setInverse, getInverse = getInverse)
}


# Computes the inverse of the special "matrix" returned by the function 'makeCacheMatrix'.
# If the inverse has already been calculated, then the function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

# Example execution
mat <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0),3,3)
mcm <- makeCacheMatrix(mat)
cs <- cacheSolve(mcm)
print(cs)
