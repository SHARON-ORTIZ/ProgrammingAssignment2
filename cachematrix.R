
# Get reverse matrix:
# The following makeCacheMatrix function is a function that caches the inverse of an array.

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL                                           # Create lodging variable
        setting <- function(t) {                                  
        x <<- t
        inversa <<- NULL
        }
        getting <- function() x                                   
        settingInversa <- function(inverse) inversa <<- inverse       
        gettingInversa <- function() inversa                         
        list(setting = setting, 
                getting = getting, 
                settingInversa = settingInversa, 
                gettingInversa = gettingInversa)
}


# The cacheSolve function calculates the inverse of the array created by 
# makeCacheMatrix. If the inverse of the input matrix has been calculated 
# before, the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        
        inversa <- x$gettingInversa()
        if (!is.null(inversa)) {
        message("getting cached data")
        return(inversa)
        }
  
         mt <- x$getting()
        if(det(mt)==0){
        return("The matrix hasn't inverse")
        }
        inversa <- solve(mt, ...)                                #getting the inverse
        x$settingInversa(inversa)
        inversa
}

# Proof makeCacheMatrix and cacheSolve functions
t <- makeCacheMatrix(matrix(sample(25),5,5))
cacheSolve(t)
