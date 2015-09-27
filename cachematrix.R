##These 2 functions allow us to cache the result of the computation
## to determine the inverse of a matrix. This calculation is usually time
## consuming and we can speed it up by using the result stored in the cache

## Function that creates a 'special' matrix that can cache its inverse
## The <<- operator assigns value to an object in a different environment
## Assuming matrix is always invertible
makeCacheMatrix <- function(myMatrix = matrix()){
        ## Create an empty matrix to store the inverse
        inverseMatrix <- NULL
        ##Sets the value of the matrix
        set <- function(y) {
                myMatrix <<- y
                inverseMatrix <<- NULL
        }
        ##Gets or 'returns' the value of the matrix
        get <- function(){ 
                myMatrix
        }
        ##Sets the value of the inverse matrix
        setInverse <- function(inverse){
                inverseMatrix <<- inverse
        }
        ##Gets or 'returns' the values of the inverse matrix
        getInverse <- function(){
                inverseMatrix
        }
        ##the list of the four functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}
## Function that calculates the inverse of the special matrix created using
## the function makeCacheMatrix. It checks to see if the inverse is in the cache
## skips computation if it finds it otherwise calculates the inverse and 
## sets it in the cache
## Assuming matrix is always invertible
cacheSolve <- function (myMatrix, ...) {
        ##tries to get the inverseMatrix from the cache
        inverseMatrix <- myMatrix$getInverse()
        ##checks if it was successful
        if(!is.null(inverseMatrix)){
                ##displays a message confirming cache retrieval
                message("getting cached data")
                return (inverseMatrix)
        }
        ## if unsuccessful, gets the matrix
        tempMatrix <- myMatrix$get()
        ## computes the inverse
        inverseMatrix <- solve(tempMatrix)
        ##sets the inverseMatrix in the cache
        myMatrix$setInverse(inverseMatrix)
        ##returns inverseMatrix
        inverseMatrix
}