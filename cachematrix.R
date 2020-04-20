## makeCacheMatrix and cacheSolve are a pair of matrix that retrives a cached result (inverse of a matrix) if there is one. 

makeCacheMatrix <- function(x=matrix()) 
{
        inverse_ <- NULL                                                             #initializing the inverse
        set <- function(matrix_)                                                     #setting the matrix
        {
                x <<- matrix_
                inverse_ <<- NULL
        }
        get <- function() x                                                          #getting the matrix
        setinverse <- function(inverse) inverse_ <<- inverse                         #setting the inverse
        getinverse <- function() inverse_                                            #getting the inverse    
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##return list of methods
}

##cacheSolve will compute new inverse if it isn't already been calculated for the matrix returned by makeCacheMatrix.
##If the inverse has already been calculated ( matrix is also unchanged), then this function would simply return the already computed inverse

cacheSolve <- function(x,...)
{
        matrix_ <- x$getinverse()                                                    #return inverse matrix of x
        if(!is.null(matrix_))
        {
                message("Getting cached data...")
                return(matrix_)                                                      #return inverse if already set
        }
        data <- x$get()                                                              #returns matrix
        matrix_ <- solve(data) %*% data                                              #Matrix multiplication to find inverse involving solve()
        x$setinverse(matrix_)                                                        #setting the new inverse
        matrix_                                                                      #return the matrix
}