## makeCacheMatrix function: Returns a list of below functions to set, get, set inverse
##                          and get inverse. The argument passed is the matrix for which
##                          inverse should be calculated


makeCacheMatrix <- function(x = matrix()) {
  
    #whenever a new matrix is passed the inversion matrix is set to null
    i<-NULL
    
    ##  set function can be use to set the matrix
    set <- function(y){
               x<<-y
              i<-NUlL
    }
    
    ## get function can be used to get the matrix that is already assigned.
    ## Because this function is called in makeCacheMatrix the value x that 
    ## that is passed to the function is assigned
    get<- function() x
    
    ##  setInverseMatrix(x) - By passing the inverse matrix as value we are
    ##  storing this value against the variable i    
    setInverseMatrix <- function(invMatrix=matrix()) i<<-invMatrix
    
    ##  The assigned value using setInverse can be fetched by this function
    getInverseMatrix <- function() i
    
    ## return the list of all the above 4 functions
    list(set=set,get=get,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)

}


## cacheSolve function: The argument passed to this function is the list of functions that
##                      has been returned from the makeCacheMatrix function. This methods returns
##                      the inverse of matrix passed originally to the makeCacheMatrix function.

cacheSolve <- function(x, ...) {

   # in the list of functions x, the getInverseMatrix function is used to assign the value
   #  to the inverse matrix "i". If the inverse is not already calculated the i will be 
   #  assigned null as the null is assigned as default value in makecachematrix
  
    i <- x$getInverseMatrix()
    
  # to check if i is null or not, if not this function returns this already existing value.
  # This function need not calculate inverse once again.
    
    if(!is.null(i))
    {
      message("getting from cache")
      return(i)
    }
    
  # if the i is null, we get the matrix using x$get (assigned to m) and find inverse using solve(m)  
    m<-x$get()
    i<-solve(m)
  
  # The inverse that is calculated is also store using setInverseMatrix()
  x$setInverseMatrix(i)
  
  # returns inverse of matrix m
  i
}

# Example Implementation

#matrix for which we want inverse is initialize in Z
z<-matrix(c(1,5,4,1:6),3,3)

#This function takes the argument of z and list the functions. Please note that at this point
# inverse variable is still NULL
s<-makeCacheMatrix(z)

#When we call cacheSolve with the list as argument, it will find that i is null and find out 
# the inverse and assign to setInverse
cacheSolve(s)

#Because inverse matrix is already assigned  when we called previously, the below call should
# fetch value from the cache
cacheSolve(s)