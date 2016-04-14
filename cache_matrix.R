makeCacheMatrix <- function(n = matrix()) {
 -
 +        inver <- NULL
 +        set <- function(m) {
 +                n <<- m
 +                inver <<- NULL
 +        }
 +        get <- function() n
 +        setInverse <- function(inverse) inver <<- inverse
 +        getInverse <- function() inver
 +        list(set = set,
 +             get = get,
 +             setInverse = setInverse,
 +             getInverse = getInverse)
  }
  
  
 -## Write a short comment describing this function
 +## This function computes the inverse of the special "matrix" created by 
 +## makeCacheMatrix above. If the inverse has already been calculated (and the 
 +## matrix has not changed), then it should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'n'
 +        inver <- x$getInverse()
 +        if (!is.null(inver)) {
 +                message("getting cached data")
 +                return(inver)
 +        }
 +        mat <- x$get()
 +        inver <- solve(mat, ...)
 +        x$setInverse(inver)
 +        inver
  }
