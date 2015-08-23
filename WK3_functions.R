
#Create a function called makeCacheMatrix which takes a square martix as a paramenter.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # defining the set function
  set <- function(y) { 
    
    # lexical scoping is used here
    x <<- y
    m <<- NULL
  }
  
  get <- function() x       # calling get function
  
  #Assigning values to setsolve and getsolve variables 
  setsolve <- function(solve) m <<- solve  
  getsolve <- function() m
  
  # Returning list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Make a cacheSolve function which gets its values from makeCacheMatrix function.
cachesolve <- function(x, ...) {
  m <- x$getsolve()
  
   #Check if value of m if already exist then get it from cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Assign values to variables and return result.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


