## Put comments here that give an overall description of what your functions do:
#makeCacheMatrix is usefull for assigning a list containing a related set of values and 
#functions related to a square matrix. This enables storage and access to the matrix and it's
#inversion, without having to recalculate the inversion each time you wish to access it.
#The cacehSolve function allows us to access the inversion as needed, with the added benefit of
#checking whether or not it is necessary to calculate the inversion beforehand. By using these
#two functions together, we can speed up our program by ensuring we are calculating matrix
#inversions as little as possible.


## Write a short comment describing this function
#The makeCacheMatrix function takes a matrix as an argument and returns a list of these elements:
#$set: A function to set a matrix as the variable in the makeCacheMatrix argument and clear
#variable m, which is meant to cache an inversion of matrix x. You would want it cleared
#in case the new matrix x has a diffrent inversion that the one it replaced. 
#$get: A function to retreive the original matrix used in the makeCacheMatrix argument.
#$setSolve: A function to assign an inverse matrix stored in cacheSolve's "solve" to variable m.
#$getSolve: A function to retreive the inverse matrix stored in variable m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## Write a short comment describing this function:
#cacheSolve is used to return the inverse matrix of a list object created using the 
#makeCacheMatrix function. It will also set the inverse matrix of that object if it has not yet 
#been set.
#The "if" statement checks for a cached inverse matrix in the list object's $getSolve elemet. 
#If that element is not null, cacheSolve simply returns the object stored in $getSolve. 
#But if that element is null, cacheSolve accesses the original matrix in the $get element and 
#uses it to generate it's matrix inversion. After generating it's inversion, cacheSolve stores 
#the inversion to the original object using the function sotred in the object's $setSolve 
#element. Finally, the result (the inverse matrix) is returned.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting catched data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setSolve(m)
        m
}