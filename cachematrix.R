## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix will create a special matrix object that can cache its inverse. We will creating functions under this function.
makeCacheMatrix <- function(x = matrix()) { 
      
        inverse<-NULL                             ##create a NULL value variable
  
      set <- function(i,col,row){                 ##set() will create a matrix and store its value in variable(matrix) i.e "x"
        x <<- matrix(i,col,row)       
        inverse <<- NULL                          ##reset the value for inverse(of matrix) to NULL.
                                                  ## "<<-" will point to the variable inverse and change its value to NULL.
      }
      
      get <- function(){                          ##get() will show the value of the matrix created by set() function.
        x
      }
      
      setinverse <- function(result){             ##setinverse() take the result calculated(inverse matrix) and set it to inverse variable(matrix).
        inverse <<- result
      }
      
      getinverse <- function(){                   ##getinverse() will show the inverse matrix.
        inverse
      }
      list(set = set,                             ##List will the list of the functions.
           get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}


##cacheSolve() will compute the inverse of the special matrix returned by makeChacheMatrix().
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            
      inverse  <- x$getinverse()              ##getinverse() will store the current value of the inverse matrix into "inverse" variable.
      if(!is.null(inverse)){                  ##check if the value of inverse(matrix) is NULL or Not.
        message("getting cached data")            ##If not, instead of calculating again. It will return the cached data(matrix).
        return(inverse)
      }
      
      m <- x$get()                            ##store the value of input matrix into variable "m".
      result  <- solve(m)                     ##create inverse of input matrix and store in variable "result".
      x$setinverse(result)                    ##x$setinverse() will take the result(matrix) value and store it in the "inverse" (matrix variable).
      
      result                                  
}
