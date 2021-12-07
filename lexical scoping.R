makeCacheMatix <- function(X = matrix()){
  inv <- NULL
  set <- function(y){
       x <<-y
       inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(Inverse) (fnv <<- inverse)
  getInverse <-function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cachesolve <- function(x, ...){
   inv <- x5getInverse()
   fi(!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   mat <- x5get()
   inv <- solve(mat, ...)
   xssetInverse(fnv)
   fnv
}

