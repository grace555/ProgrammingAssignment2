# following are two functions are used to cache the inverse of a matrix.
## saving us the hassle of computing it repeatedly and thus saving us time
# by getting the matrix last computer inverse from the cahe

# makeCacheMatrix creates a set of functions to help us do the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matInv <<- inverse
    getinverse <- function() matInv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# The following function returns the inverse of the matrix from the above function(makeCacheMatrix()).
#first , it checks if
# the inverse has already been computed. If so, it gets the result and ends
# If not, it computes the inverse, stores the value in the cache via
# setinverse function.
# this function assumes that the matrx is a pair matrix and thus invertible
cacheSolve <- function(x, ...) {
    matInv <- x$getinverse()
    if(!is.null(matInv)) {
        message("getting cached data.")
        return(matInv)
    }
    data <- x$get()
    matInv <- solve(data)
    x$setinverse(matInv)
    matInv  ## Return a matrix that is the inverse of 'x'
}

#myTest/ a sample of  a few runs

#run1 _ a 2x2 matrix
#x = rbind(c(2, -1/8), c(-2/4, 1))
# m = makeCacheMatrix(x)
# m$get()
#     [,1]   [,2]
#[1,]  2.0 -0.125
#[2,] -0.5  1.000
# cacheSolve(m)
#         [,1]       [,2]
#[1,] 0.5161290 0.06451613
#[2,] 0.2580645 1.03225806

#run2_ a 3x3 matrix
#x = rbind(c(2, -1/8,8/5), c(-2/4, 1,1/2), c(4/5, -5/8,1/7))
# m = makeCacheMatrix(x)
# m$get()
#     [,1]   [,2]      [,3]
#[1,]  2.0 -0.125 1.6000000
#[2,] -0.5  1.000 0.5000000
#[3,]  0.8 -0.625 0.1428571
# cacheSolve(m)
 #         [,1]      [,2]      [,3]
#[1,]  6.343284 -13.68159 -23.15920
#[2,]  6.567164 -13.85075 -25.07463
#[3,] -6.791045  16.01990  26.99005
#

#run3 _ a 4x4 matrix
#x = rbind(c(2, -1/8,8/5,4/5), c(-2/4, 1,1/2,17/19), c(4/5, -5/8,1/7,2/7), c(4, -7/9, 4/6,9/17))
# m = makeCacheMatrix(x)
# m$get()
#     [,1]       [,2]      [,3]      [,4]
#[1,]  2.0 -0.1250000 1.6000000 0.8000000
#[2,] -0.5  1.0000000 0.5000000 0.8947368
#[3,]  0.8 -0.6250000 0.1428571 0.2857143
#[4,]  4.0 -0.7777778 0.6666667 0.5294118
# cacheSolve(m)
#           [,1]        [,2]       [,3]       [,4]
#[1,] -0.1389956  0.02340787 -0.4473197  0.4118877
#[2,] -0.1594319  0.37079467 -1.5067323  0.4274117
#[3,]  1.0214316 -0.49743621 -0.3824306 -0.4964092
#[4,] -0.4702854  0.99428941  1.6477334  0.0298822


