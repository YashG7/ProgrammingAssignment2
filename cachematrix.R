## The makeCacheMatrix funtion creates a special "matrix" object that can cache its inverse.
## The cacheSolve function returns the inverse of the matrix object by makeCacheMatrix

## The makeCacheMatrix function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()) 
{
  inverse<-NULL
  getmatrix<-function()
  {
    a
  }
  setmatrix<-function(b)
  {
    a<<-b
    inverse<<-NULL
  }
  getinverse<-function()
  {
    inverse
  }
  setinverse<-function(inverse_solution)
  {
    inverse<<-inverse_solution
  }
  list(getmatrix=getmatrix,setmatrix=setmatrix,getinverse=getinverse,setinverse=setinverse)
}

## This  following function computes the inverse of the matrix returned by makeCacheMatrix function
cacheSolve <- function(a, ...) 
{
  inverse<-a$getinverse()
  if(!is.null(inverse))
  {
    return(inverse)
  }
  inverse<-solve(a$getmatrix())
  a$setinverse(inverse)
  inverse
}