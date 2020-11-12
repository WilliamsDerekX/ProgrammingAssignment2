## Put comments here that give an overall description of what your
## functions do
## Derek Williams  Nov 11 2020
## The makeCacheMatrix function will take a SQUARE numeric matrix and
## return a list of the matrix entered stored in cache, and
## the inverse of the matrix if previously calculated by the first call of
## cacheSolve, otherwise initially stored as NULL in cache.
## cacheSolve will take the list returned by makeCacheMatrix
## and either pull from cache if an inverse had been previously calculated
## or calculate the inverse of the matrix entered from the list and
## call functions withing makeCacheMatrix to store the inverse in cache 


## Write a short comment describing this function
## makeCacheMatrix takes a square matrix assumed to be invertible
## then with a series of functions sets the input in cache and clears
## the cached_inv if called, gets the input, stores the inverse in cache
## and gets the inverse stored

makeCacheMatrix <- function(input = matrix()) {
        cached_invX <- NULL
        set_input <- function(y){
                input <<- y
                cached_invX <<- NULL
        }
        get_input <- function() input
        set_invX <- function(inversed_matrix) cached_invX <<- inversed_matrix
        get_invX <- function() cached_invX
        list(set_input = set_input, get_input = get_input, set_invX = set_invX, get_invX = get_invX)
}


## Write a short comment describing this function
## cacheSolve checks to see if an inverse is stored in cache
## calling a function from makeCacheMatrix, then either retrieves
## the cached inverse or calculates an inverse of the entered matrix
## if not present,  The inverse is calculated using R's solve function
## which is enclosed in the try() function because not all square
## matrices are invertible as a check.This function the returns the
## inverse into cache calling the makeCachedMatrix function and
## returns the inverse as an output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cached_invX <- x$get_invX()
        if(!is.null(cached_invX)){
                message("getting cached data")
                return(cached_invX)
        }
        data <- x$get_input()
        cached_invX <- try(solve(data, ...))
        if(class(cached_invX)[1] == "try-error"){
                message("error: matrix entered in possibly singular or not invertible")
                cached_invX <- NULL
        }
        x$set_invX(cached_invX)
        cached_invX
}
