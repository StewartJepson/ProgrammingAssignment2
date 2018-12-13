## The following functions cache inverse of a matrix

## makeCacheMatrix stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {  # x is initialized as a function argument 
        I <- NULL   # I is initialized as a NULL object so it can be used later in the function 
        set <- function(y) {
                x <<- y # object y is assigned to x, in the parent environment
                I <<- NULL # the value of NULL is assigned to I to clear any previously cached value
        }
        get <- function() x # x is retireved from the parent environment
        setI <- function(inverse) I <<- inverse # input argument is assigned to I in the parent environment 
        getI <- function() I # getI retrieves the value of I
        list(set = set, get = get,
             setI = setI,
             getI = getI) # makeCacheMatrix returns a list of the defined functions 
}


## cachesolve gets the vlaue of I, checks if it is NULL
## returns cached value if it exists, calculates inverse if it does not

cacheSolve <- function(x, ...) {
        I <- x$getI() # call getI function to retrieve I from the input object that was created by makeCahceMatrix
        if(!is.null(I)) { # check if I exists (i.e. is not NULL)
                message("getting cached data")
                return(I) # return cached value if not NULL
        }
        data <- x$get() # assign the input matirx to data using get()
        I <- solve(data, ...) # use the solve function to compute the inverse of the matrix
        x$setI(I) # set I in the input object (created by makeCacheMatirx)
        I # Print I
}
