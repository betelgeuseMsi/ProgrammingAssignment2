## We create a special matrix object that can cache its inverse
## First initialize two objects 'x' and 'inv'
## Set the value of the matrix and then get its value
## Later set the value of the inverse and then get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(matInv) inv <<- matInv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function will compute the inverse of the special matrix created by 
## makeCacheMatrix above.If it's already been calculated and matrix is same,
## it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
