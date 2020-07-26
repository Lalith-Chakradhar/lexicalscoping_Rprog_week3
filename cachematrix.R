##makeCacheMatrix function for taking new input matrix
##and recording the previously calculated inverse matrix for one set of values

makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  set<-function(mtr){
    x<<-mtr
    xinv<<-NULL
  }
  get<- function() x
  setinv<-function(inverted) xinv<<-inverted
  getinv<-function() xinv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##cacheSolve function is used to first check whether the inverse matrix for one set of values
##is calculated or not,if inverse already calculated it prints that(skipping computation) or
##calculates the inverse for new set of matrix values and prints it

cacheSolve <- function(x, ...) {
      xinv<-x$getinv()
      if(!is.null(xinv)){
        message("getting cached inverted matrix")
        return(xinv)
      }
      data <- x$get()
      xinv<-solve(data,...)
      x$setinv(xinv)
      xinv
}
