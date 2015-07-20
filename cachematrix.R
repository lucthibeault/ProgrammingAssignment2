## Pair of functions used to manage data caching for matrix inversion operations
## The assumption is made that the matrix is always a square invertible matrix

## The functions are "makeCacheMatrix" to prepare the caching object and 
## "cacheSolve" to resolve and cache the operation. If the operation has been
## executed in the past, the cached result will be returned

## Creates an "cachable matrix" object for the inversion operation.
## Parameters: 
##              currMatrix: Matrix to be inverted. Default value: Empty matrix
makeCacheMatrix <- function(currMatrix = matrix()) {
    ## Cached result of the last computation
    result <- NULL
    
    ## Sets the matrix to be computed and clears the cached result
    set <- function (newMatrix)
    {
        currMatrix <<- newMatrix
        result <<- NULL
    }
    
    ## Gets the current matrix to be computed
    get <- function ()
    {
        currMatrix
    }
    
    ## Sets the cached result
    setResult <- function (newResult)
    {
        result <<- newResult
    }
    
    ## Gets the cached result
    getResult <- function ()
    {
        result
    }
    
    list(set = set, get = get, setResult = setResult, getResult = getResult)
}

## Returns the inverted matrix of the specified matrix
## Parametrs:
##              cacheMatrix: Cached Matrix object, as returned by the
##                           "makeCacheMatrix" function
cacheSolve <- function(cacheMatrix, ...) {
    cacheRes <- cacheMatrix$getResult()
    
    if (!is.null(cacheRes))
    {
        message("Getting cached data")
        return(cacheRes)
    }
    
    dataExec <- cacheMatrix$get()
    cacheRes <- solve(dataExec, ...)
    
    cacheMatrix$setResult(cacheRes)
    cacheRes
}
