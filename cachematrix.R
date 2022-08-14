## This pair of functions helps us create a special matrix whose inverse is
## calculated only if it hasn't already been calculated. In case it has been
## calculated before, we simply return the inverse already stored as cache

## This function takes a matrix object as input as returns a list of four functions
## that help us set its value, get its value, set the inverse and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv<-NULL
    
    set<-function(y=matrix())
    {
        inv<<-NULL ##If we're setting a new value we need to reset the inverse too
        x<<-y
    }
    
    get<-function()
    {
        x
    }
    
    setinv<-function(i)
    {
        inv<<-i
    }
    
    getinv<-function()
    {
        inv
    }
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function is for getting the inverse of the special matrix created above.
## If it hasn't been calculated already (i.e. it is NULL) it calculates it and stores
## it using setinv(). Otherwise, it simply returns the already calculated value.

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    
    if (!is.null(inv))
    {
        print("Getting inverse from cached data")
        return(inv)
    }
    
    data<-x$get()
    inv<-solve(x$get())
    x$setinv(inv)
    
    inv
}
