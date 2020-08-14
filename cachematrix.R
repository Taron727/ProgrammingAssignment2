## creates a special "matrix" object that can cache its inverse
## param:  x =a matrix
## return: a list of setters and getters 
makeCacheMatrix <- function(x = matrix()) {
    # if matrix not invertible then return empty list 
    if(!"matrix" %in% class(try(solve(x), silent=T))) {
        message("[ERROR]: matrix cannot be inverted")
        return(list())
    }
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}
## computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
## param: 	x =a list(should be created using makeCacheMatrix())
##			data =a matrix of latest state; used to compare with cached
## return:  an inverted matrix or empty matrix if data
cacheSolve <- function(x, data=matrix(), ...) {
    # if data diff from cache, then remake
    # if matrix not invertible then return 0
    if(!identical(data, x$get())){
        message("[WARNING: cached data different. updating...")
        x <- makeCacheMatrix(data)
		if(length(x)==0) return(matrix()) 
    }
                
    m <- x$getinv()
    if(!is.null(m)) {
        message("[MESSAGE]: getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}