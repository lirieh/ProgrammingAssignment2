## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix function allows us to store a matrix and its inverse matrix.
#Get and set are used to get the value of the matrix
#Getinv and setinv are used to get and set the inverse matrix values
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

#cacheSolve checks for the cached inverse matrix and calculates a new inverse matrix
#if there is no stored inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setinv(i)
        i
}

