## The following two functions (makeCacheMatrix() and cacheSolve()) work 
## together in order to get the inverse of a matrix (if possible) and store in the cache.
## This is useful if the same big matrix has to be "inverted" several times.
## The operation (obtained with solve()) can take some time in case of big matrices and saving
## a result in the cache save time.
## The assignment seems to be meant to teach the scoping in R and the use of "<<-" to "penetrate" the 
## working environment and assign a value to an object in the parent environmet.
## I think that this assignment could be better organized (if the aim is what I get). I get lost in the
## operations of the functions before getting the scope of the assignment... 

## This function creates a list of objects (functions) later used by the cacheSolve() function to 
## calculate the A^-1 matrix created in the function itself. The environment within the object created 
## with this function contains:
## -4 functions (set, get, setinverse and getinverse)
## -2 objects (the original matrix, here X, and an empty object, I, where the inverted matrix will be stored later)

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL #define the object where the inverted matrix will be
    set <- function(y) { #his function to set the values for the function (and also to "clean" I, in case something is cached)
        x <<- y
        I <<- NULL
    }
    get <- function() x #this function just gives the input matrix
    setinverse <- function(inverse) I <<- inverse #this function will put the inverted matrix in the empty I (with the next part of code,cacheSolve())
    getinverse <- function() I #get it
    #below a list is crated (the output of the function) with the NAMED objects that contains the functions used by cacheSolve()
    list(set = set, #"store" the function set in an object named set in the list
         get = get, #as above
         setinverse = setinverse, #as above
         getinverse = getinverse) #as above
}


## The below function works on an object created with makeCacheMatrix(). 
## It uses the objects (function listed in a list, the original to be inverted matrix x, and 
## the so far empty object I) within the output of makeCacheMatrix().
## It returns a matrix that is the inverse of 'x' and "chaches" it in I (which is inside the 
## environment of the result given by makeCacheMatrix). If I is not empty (i.e. the
## same matrix has been already inverted and "cached") it gives the stored result.
## Otherwise it calculates the inverted matrix and (using the functions listed in the argument of the 
## function itself) stores the result in the cache. Sounds complicated. It is. Or was.

cacheSolve <- function(x, ...) {
            I <- x$getinverse() 
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}

###test it out
myMatrix<-makeCacheMatrix(matrix(runif(25),5,5))
myMatrix$get()
cacheSolve(myMatrix)
