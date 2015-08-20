## This file implements a "special matrix" object which supports caching
## of the matrix's inverse/solution.  makeCacheMatrix is used to create
## "special matrix" objects.  cacheSolve computes the inverse of a
## "special matrix" created by makeCacheMatrix.
##
##  Requires the digest library

library(digest)

## Generate an MD5 fingerprint value for an object
md5fingerprint <- function(obj) {
   return(digest(obj, algo="md5", serialize=TRUE))
}


## makeCacheMatrix creates a "special matrix" object (implemented as a list)
## which supports caching of the matrix's inverse/solution. getters and
## setters are provided similar to those found in object oriented programming.
makeCacheMatrix <- function(x=matrix()) {
   solution <- NULL # Default to not having a cached inverse for this "special matrix"
   fingerprint <- md5fingerprint(x) # Remember the source matrix's fingerprint

   # Setter method to store the source matrix passed in.  The cached matrix solution
   # is defaulted to NULL until the matrix has been solved the first time.
   set <- function(y) {
             x <<- y # Save the source matrix's data
             solution <<- NULL # Not been calculated yet
             fingerprint <<- md5fingerprint(y) # Fingerprint the source matrix
   }

   # Getter method to return the source matrix
   get <- function() x

   # Setter method to cache the matrix's inverse
   setinverse <- function(inverse) solution <<- inverse

   # Getter method to return the matrix's cached inverse
   getinverse <- function() solution

   # Getter method to return the source matrix's fingerprint
   getfingerprint <- function() fingerprint

   # Return back the "special matrix" (list) object to the caller (al a constructor style)
   list(set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse,
        getfingerprint=getfingerprint)
}


## cacheSolve computes the inverse of a "special matrix" created by makeCacheMatrix.
## If an inverse has already been calculated and the matrix hasn't changed, then the
## cached solution is returned.  If a solution hasn't been calculated, or if the
## matrix has changed, then the solution is recomputing again from scratch and cached.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x' either by calculating it or getting it
   ## from cache.

   # Check if the solution has already been computed and cached.
   # Fingerprinting is used to detect changes to the source matrix
   # in the list context after the list is created.  People shouldn't
   # be messing with the list, but just in case...  An alternative
   # would be to lock the matrix's binding, but that would be sneaky
   # and more than is needed here.
   solution <- x$getinverse() # Try to get an already cached solution
   originalfp <- x$getfingerprint() # Get original fingerprint
   currentfp <- md5fingerprint(x$get()) # Get current data's fingerprint
   if ((currentfp == originalfp) && (!is.null(solution))) { # Not modified and already cached?
      # print("returning cached data")
      return(solution) # Return the cached solution
   }

   # Not already cached or has been messed with (bad user!).
   # It looks like we're going to have to do this the hard way...
   data <- x$get() # Get the source matrix data
   solution <- solve(data, ...) # Compute the solution
   x$setinverse(solution) # Cache the solution

   return(solution) # Return solution to the caller
}
