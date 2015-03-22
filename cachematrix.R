## makeCacheMatrix stores a matrix and its inverse on a list.
##
## cacheSolve searches the matrix in the cache created by makeCacheMatrix and
## returns the stored inverse if it finds it. If it does not find it, the program
## will calculate and return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x = x^-1
  cache = list(x,inv_x)
}


cacheSolve <- function(x, ...) {
  
  cache = makeCacheMatrix(x)
  len = length(cache)
  achou = 0
  
  for (i in 1:len){
    if (cache[[i]] == x){
      inv = cache[[i+1]]
      achou = 1
      break
    }else
      i = i+2
  }
  
  if (achou == 0){
    inv = x^-1    
  }

  inv
}
