## This function make a list which contains operations with matrix

##set(y) - function for setting new matrix, after using this function 
##it is nessesary to find inverse of matrix again becouse 
##it will be NULL after setting

## get() - function return matrix x

##setInverse(inverse) - setting value of inverseMatrix for matrix x

##getInverse() - function return inverse matrix for x 
##or NULL if it hasn`t calculated yet 

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function return inverse matrix of matrix x
## At first It detects If matrix x has cached inverse matrix of x
## It will return It. But If it hasn`t cachet inverse matrix of x
## function will calculates it, caches and returns it

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
