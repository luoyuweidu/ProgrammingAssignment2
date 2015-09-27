## makeCacheMatrix can cache the already calculated inverse of the given matrix. When we need it, we use the cacheSolve function to retrive the inverse we calculated before.
##

## cache the inverse

makeCacheMatrix <- function(x = matrix()) {
     inv = NULL
     set = function(y) {
            x <<- y
            i <<- NULL
           }
       get = function() x
       setinv = function(inverse) inv <<- inverse
       getinv = function() inv
       list(set=set, get=get, setinv=setinv, getinv=getinv)
    }

## use cacheSolve(makeCacheMatrix(x=matrix))to get the inverse we cache before.
cacheSolve <- function(x, ...) {
           inv = x$getinv()
           if (!is.null(inv)){
                 message("getting cached data")
                 return(inv)
             }
         data = x$get()
         inv = solve(data, ...)
         x$setinv(inv)
    
         return(inv)

}



