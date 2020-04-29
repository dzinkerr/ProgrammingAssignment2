## Put comments here that give an overall description of what your
## functions do:
## Calculating the inverse of a matrix tends to be difficult and costly in terms of efficiency.
## This short program lets you input a matrix,store it, and cache its inverse.

## Write a short comment describing this function:
## This function receives a Matrix and returns a list that is actually a function that lets you
## set and get the value of a Matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    set_inverse<- function(inverse) inv<<-inverse
    get_inverse<- function() inv
    list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## This function cheks if a Matrix?s inverse has already been calculated. If it has, it retreaves
## the inverse, else it calculates it. 

cacheSolve <- function(x, ...) {
    inv<-x$get_inverse()
    if(!is.null(inv)){
        message("getting cached dat")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$set_inverse(inv)
    inv
}
