#############################################################################
#                                 Coursera                                  #
#                                                                           #
#                          Johns Hopkins University                         #
#                            R Programming Course                           #
#                             Roger D. Peng, PhD                            #
#                                 March, 2015                               #
#############################################################################

#        ==============    Programming Assignment 2   ==============        #

# The second programming assignment requires to write an R function able to #
# cache the inverse of a matrix to avoid compute it repeate repeatedly.     #
#                                                                           #
# This solution is based on the example shown in the assignment example.    #
# After implementatiom the concept and fuction was tested as follows:       #
#                                                                           #
# - A 1000x1000 matrix is defined.                                          #
# - It is randomly filled with 0s an 1s.                                    #
# - Time to get the inverse is checked.                                     #
# - Matrix is cached and its inverse is calculated using the function which #
#   checks if the inverse was calculated.                                   #
# - Time to get the inverse is checked.                                     #
#                                                                           #
# > m<-matrix(0, ncol=1000, nrow=1000)                                      #
# > m<-apply(m, c(1,2), function(x) sample(c(0,1),1))                       #
# > system.time(solve(m))                                                   #
# user  system elapsed                                                      #
# 1.884   0.005   1.982                                                     #
# > system.time(solve(m))                                                   #
# user  system elapsed                                                      #
# 1.918   0.004   1.923                                                     #
# > x <- makeCacheMatrix(m)                                                 #
# > system.time(cacheSolve(x))                                              #
# user  system elapsed                                                      #
# 1.868   0.004   1.872                                                     #
# > system.time(cacheSolve(x))                                              #
# getting cached data                                                       #
# user  system elapsed                                                      #
# 0       0       0                                                         #
# ========================================================================= #

# makeCacheMatrix.- This function creates a special "matrix" object that    #
# can cache its inverse. It receives a matrix and create a list of          #
# functions that "get" and "set" the value of the matrix and its inverse    #
# (using the "solve()" buit-in function of R).                              #

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvm <- function(solve) m <<- solve
        getinvm <- function() m
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}


# cacheSolve.- This function calculates the inverse of a matrix. It first    #
# checks to see if it has already been calculated. If so, it gets the        #
# inverse from the cache and skips the computation. Otherwise, it calls the  #
# solve() function sets the value in the cache via the setinvm function      #
# defind in makeCacheMatrix().                                               #

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvm()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvm(m)
        m
}

#############################################################################