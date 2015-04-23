## This is a cached version of "solve", which computes the inverse of a square matrix.
## That is to say, "cachedSolve" skips lengthy computation if they were done before.

## We implement this with 2 functions:
## 1. "makeCacheMatrix" creates a "cached version" of a matrix x.
##      Essentially, this is an object containing x and, if provided, its inverse.
## 2. "cacheSolve" returns the inverse of the "cached version" of x. 
##      This inverse is either found in the "cached version" of x, or computed first.



## Creates a "cached version" of a matrix. It is a list containing 4 functions:
## 1. set(y) -- replaces the value of x by the value of y
## 2. get() -- returns the value of x
## 3. setInverse(y) -- stores the value of y as being the inverse of x 
## 4. getInverse() -- returns the value of the inverse if present, NULL otherwise

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) xInv <<- inverse
        getInverse <- function() xInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of a "cached matrix" x.
## If a cached value for the matrix is present, cacheSolve returns that value.

cacheSolve <- function(x, ...) {
        ## We check if the inverse exists in the "cached matrix".
        ## If so, we return that value.
        xInv <- x$getInverse()
        if (!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
        }
        
        ## If xInv is NULL (ie, there is no cached inverse):
        ## 1. get the actual matrix from the "cached matrix",
        ## 2. compute its inverse,
        ## 3. cache the result by storing it in the correct slot of the "cached matrix",
        ## 4. and return this value as the inverse of x.
        
        mat <- x$get()
        xInv <- solve(mat, ...)
        x$setInverse(xInv)
        xInv
}
