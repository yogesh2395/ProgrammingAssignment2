#The function calculates inverse of a matrix and if the inverse has been calculated 
#then instead of calculating again reads the cache value! Reduces calcuation load.

#The function creates a special matrix which consists 
#of list of functions to store values of matrix get its values,
# set the inverse and get the inverse!

makeCacheMatrix <- function(x = matrix())
{    
  sz<-dim(x)
  inv<-matrix(rep(NA),sz[1],sz[2])
  set<-function(y,mode=matrix()){
    x<<-y
    inv<<-matrix(rep(NA),sz[1],sz[2])
  }
  get<-function() x
  setinv<-function(invert=matrix()) inv<<-invert
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}  


# This function returns the inverse of a matrix! 
#If it has been calculated before returns the cache value 
#otherwise calculates the inverse!

cacheSolve <- function(t,...){
  neg<-t$getinv()
  print(neg)
  if(all(!is.na(neg)))
  {message("Getting From Cached Data")
    return(neg)
  }
  val<-matrix()
  val<-t$get()
  neg<-solve(val)
  t$setinv(neg)
  neg
}