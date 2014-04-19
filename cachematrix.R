## Function returns the inverse of a square matrix. It does not check that the matrix
## is invertible.
## The function first calls the cache function to see if the result has already been 
## computed and either returns the cache or computes the inverse
## The function call is: cacheSolve(makeCacheMatrix(x))
## where x is the invertible matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               # initializes m
        set <- function(y) {
                x <<- y         # x survives the end of the function to pass to next
                m <<- NULL      # resets m to NULL and survives the end of the function
        }
        get <- function() x     # returns just x
        setinverse <- function(solve) m <<- solve #the computed value is set into the list here
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()  #gets the matrix m from above and tests if it's NULL
        if(!is.null(m)) {    #if not NULL, returns its value
                message("getting cached data")
                return(m)
        }
        data <- x$get()     #just gets x from above
        m <- solve(data, ...)  #compute here
        x$setinverse(m)    #sets the cache in the function setinverse() above
        m
}
