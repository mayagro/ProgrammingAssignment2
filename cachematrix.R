#The makeCacheMatrix function creates a matrix object that can cache 
#the input matrix and it's inverse.
#Doing so helps R allocate computing memory more efficiently 
#for future computations operated on the same matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # sets the value of m to NULL
        set <- function(y) {   #set the value of the matrix
                x <<- y    ## caches the inputted matrix
                m <<- NULL ## sets the value of m (the matrix inverse if used cacheSolve) to NULL  
        }
        get <- function() x   #gets the value of the matrix
        setmatrix <- function(solve) m <<- solve  #solve(x) returns its inverse
        getmatrix <- function() m 
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)  #creates a list to store the 4 functions
}



##If the function works correctly, you should be able to pass the special matrix 
#(made by the function makeCacheMatrix) to cacheSolve the first time without any messages
#however, after the first time it should give you the message "getting cached data", until 
#you clear your setmatrix function assignment.
cacheSolve <- function(x = matrix(), ...) {  #comparing matrix with previous one
        m <- x$getmatrix()  #this gets the inverse if it was already calculated
        if(!is.null(m)) {   #checks if cacheSolve has been run before
                message("getting cached data") #if it has then provides this message
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #computes the inverse of the input matrix
        x$setmatrix(m) #runs the setinverse function on the inverse to cache the inverse
        m #return the inverse
}
