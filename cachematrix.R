#This function creates an object that can cache 
#This will help use memory more efficiently 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # sets value of m to NULL
        set <- function(y) {   #sets value of the matrix
                x <<- y    ## caches  matrix which was inputed
                m <<- NULL ## sets to null m value   
        }
        get <- function() x   #gets value for the matrix
        setmatrix <- function(solve) m <<- solve  #returns its inverse
        getmatrix <- function() m 
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)  #creates a list to store the 4 functions
}



##If the function works correctly, you should be able to pass the special matrix 
# to cacheSolve the first time without any messages
cacheSolve <- function(x = matrix(), ...) {  #comparing matrix with one before it
        m <- x$getmatrix()  #this gets the inverse if the inverse was already calculated
        if(!is.null(m)) {   
                message("getting cached data") #if m was run before it will provide message
                to return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #computes the inverse 
        x$setmatrix(m) #runs the setinverse function on the inverse to cache the inverse
        m #return the inverse
}
