## makeCacheMatrix accepts a matrix returns a list of getter and setter
## functions to allow access to it and an inverse that will be computed in
## a separate function.
makeCacheMatrix <- function(x = matrix())
{
    
    # create a variable to hold the InverseMatrix.
    InverseMatrix <- NULL
    
    # create a set of functions the purpose of which is to assign
    # the initial values of the matrix and its inverse.
    #   1. set :  allows manipulation of the parent routine's variables.
    #             It can be debated whether this is a good idea or not.
    #             We don't use it.
    #   2. get :  returns the matrix passed to makeCacheMatrix.
    #   3. getinverse:  retrieves the inverted matrix from the parent
    #             routine's variables.
    #   4. setinverse:  caches the inverted matrix using the parent routine's
    #             variables.
    set <- function(y)
    {
        # using the <<- operator "causes a search to be made through parent
        # environments for an existing definition of the variable being
        # assigned."
        x <<- y
        InverseMatrix <<- NULL
    }
    get <- function()
    {
        x
    }
    setinverse <- function(invertedmatrix)
    {
        InverseMatrix <<- invertedmatrix
    }
    getinverse <- function()
    {
        InverseMatrix
    }
    
    # this list is returned to the calling function.
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## Returns a matrix that is the inverse of 'x'.  If it exists in the
## cache, return that one; otherwise, use solve() to calculate the
## inverse.
cacheSolve <- function(x, ...) {
    
    # first, get the inverse from parent environment using the getinverse()
    # function.  If something was returned, then it is the inverse of the
    # matrix and we are done.
    InverseMatrix <- x$getinverse()
    if (!is.null(InverseMatrix))
    {
        message("getting cached data")
        return(InverseMatrix)
    }
    
    # since the inverse didn't exist, we use get() to obtain the matrix from
    # the parent environment.  Then, we usel solve() to caluclate the inverse.
    # Before we return the inverse, we cache it using setinverse().  Finally,
    # we return the inverse of the matrix.
    data<-x$get()
    InverseMatrix <- solve(data)
    x$setinverse(InverseMatrix, ...)
    InverseMatrix
    
}

