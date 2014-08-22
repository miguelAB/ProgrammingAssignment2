## Esta función crea una matriz "especial"
## apartir de una matriz dada
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                   
  set <- function(y){             
    x <<- y                     
    m <<- NULL
  }
  
  get <- function()x                          
  setmatrix <- function(matrix) m <<- matrix  
  getmatrix <- function() m                   
  
  list(set=set, get=get,                
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}## End function

## Esta función calcula la inversa de una 
## matriz creada con la función anterior
cacheSolve <- function(x, ...) {
          
  m <- x$getmatrix()    
  if(!is.null(m)){      
    message("getting cached data")  
    return(m)
  }
  
  data <- x$get()       
  m <- solve(data, ...) 
  x$setmatrix(m)        
  m           
}## End function