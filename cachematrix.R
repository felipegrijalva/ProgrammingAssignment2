## Put comments here that give an overall description of what your
## functions do

# CacheSolve returns the inverse matrix of a "special"
# matrix (created using makeCacheMatrix) that can cache its inverse. 


## Write a short comment describing this function

#makeCacheMatrix creates a matrix that can cache its inverse.
#The inverse matrix is stored in the variable inv (the cache)
#It returns a list containing 4 functions:
# 1.set: set the value of the matrix
# 2.get: get the value of the matrix
# 3.setinverse: set the inverse matrix
# 4.getinverse: get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #cache = NULL
        set <- function(y){ #set matrix data
                x <<- y
                inv <<- NULL ## set cache to NULL when matrix has changed.
        }
        get <- function() x #return matrix data
        setinverse <- function(inverse) inv <<- inverse #set inverse data
        getinverse <- function() inv #get inverse data
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #return a list of functions to manipulate the matrix
        
}


## Write a short comment describing this function

#cacheSolve returns the inverse matrix of a "special matrix" (defined using makeCacheMatrix).
# If the inverse has already been calculated (and the matrix has not changed), 
# then CacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) { #if the inverse has already been calculated
                message("getting cached data")
                return(inv)
        }
        # if the inverse has not been calculated:
        data <- x$get() #get matrix data
        inv <- solve(data, ...) #calculate the inverse 
        x$setinverse(inv) #set inverse data
        inv #return inverse
}
