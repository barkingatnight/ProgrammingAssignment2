## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.


## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
##   set the value of the matrix
##   get the value of the matrix
##   sets the value of the inverse
##   gets the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample
## dat<-rbind(c(1,2), c(2,1))
## 1. CREATE MATRIX
## mat1<-makeCacheMatrix(dat)
## 2. DISPLAY MATRIX
## mat1$get()
##     [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## 3. MAKE INVERSE
## cacheSolve(mat1)
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## 4. GET CACHED INVERSE VALUE
## cacheSolve(mat1)
## getting cached data.
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333