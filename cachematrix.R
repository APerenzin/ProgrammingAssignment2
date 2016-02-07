#week 3 programming assignment
#lexical scoping in R

#this code contains two functions: 
        # 1) makeCashematrix 
        # 2) casheSolve

#the first function (makeCashematrix) creates a special "matrix" object that can cashe it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse #assign value to inv in differnt environment
        getInverse <- function() inv
        #next, list containing functions to 
                # 1- set the value
                # 2- get the value
                # 3- set the value of the inverse matrix
                # 4- get the value of the inverse matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
#the second function (casheSolve) computes the inverse returned by makeCashematrix
#if the inverse has already been created, the casheSolve function 
#will retrieve the inverse that was previously calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {    #if statement to see if inverse exists
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}