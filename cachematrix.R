## cacheMatrix.R
## This program contains two functions. makeCacheMatrix() and cacheSolve()
## Each of these functions are described below.
## By Luis F. Montoya
## April 26, 2014
## USAGE:
## 1. First generate an invertible matrix ex: a <- matrix(runif(5^2), 5)
## 2. Generate list 'b' as: b <- makeCacheMatrix(a)
## 3. Find the inverse of 'a' using this call: c <- cacheSolve(b)
## 4. Verify that the cache is working by calling cacheSolve(b) again.
##    Should get message "getting cached data" followed by 'a's inverse
## 5. Replace the original matrix inside list 'b' using this command:
##    b$set(matrix(runif(5^2), 5))
## 6. Issuing command c <- cacheSolve(b) recalculates the inverse of 
##    matrix. 
## 7. Verify that the inverse is obtained by issuing the command:
##    round(b$get() %*% cacheSolve(b), 2)
##    The result should be a 5x5 Identity Matrix
##

## makeCacheMatrix()
## This function takes an invertible matrix as argument and generates a
## list of 4 functions: set(), get(), setinv(), and getinv().
## The implementation of each of these functions is explained in the listing.
## In order to simplement the cacheing system, this function initializes 
## the desired matrix inverse (m) to NULL. This result is also initialized by 
## the set() function described below, to indicate that the cache is empty.
## Usage: b <- makeCacheMatrix(a), where a is an invertible matrix.
## b is now a list of the 4 functions listed above.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set() is used to change the value of the matrix used when the 
        # makeCacheMatrix() function is used. Since the matrix is different,
        # the result m is initialized to NULL
        # b$set(c), where c is an invertible matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get() is used to obtain the original matrix a
        # Usage: b$get() - returns the 'a' matrix 
        get <- function() x
        
        # setinv() uses the solve() function to calculate the inverse of 'a'
        # The <<- operator indicates that m is being used in another environment
        # i.e cacheSolve().
        setinv <- function(solve) m <<- solve
        # getinv() returns the inverse of matrix a 
        # Usage: b$getinv(), returns the inverse of 'a', if cacheSolve(b) has 
        # been called and NULL otherwise.
        getinv <- function() m
        
        # The following line returns a list of the 4 functions described above.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Return a matrix that is the inverse of 'x'. If the inverse of 'x' has
## already been calculated, then returns the value stored in m. However,
## if this inverse has not been calculated, because is the first time this 
## function is being called or the value of 'x' has been updated with the 
## function set(), then the inverse is calculated as indicated below. This is
## one way of implementing a cache system.
## Usage: c <- cacheSolve(b)
## To verify that 'c' is the inverse of 'a' the following line
## round(c %*% a, 2)
## should return the Identity Matrix of same dimension of 'a'

cacheSolve <- function(x, ...) {
        # 'm' is assigned a matrix that is the inverse of 'x', if it has  been
        # calculated or NULL otherwise
        m <- x$getinv()
        # If 'm' is not NULL, retur its value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if m == NULL, then calculate the inverse of matrix 'a'
        # First, get the matrix 'a' and assign it to the variable 'data'
        data <- x$get()
        # Get the inverse of matrix 'data' and assign it to 'm'
        m <- solve(data, ...)
        # Set the environment of variable 'b' in the function makeCacheMatrix
        # with the inverse calculated here
        x$setinv(m)
        # return the 'm' as the inverse of matrix 'a' 
        m
}
