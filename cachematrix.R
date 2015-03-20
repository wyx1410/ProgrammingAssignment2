## For this assignment, I set up two functions: makeCacheMatrix and cacheSolve. 
## The function "makeCacheMatrix" creates a special list that can cache its inverse.
## The function "cacheSolve" computes the special "matrix" obtained by "makeCacheMatrix". 
## If the inverse has already been caclulated(and matrix does not change), then the cachesolve should retrieve the inverse from the chache. 


## makeCacheMatrix:contains 4 member functions:
## a) set a matrix b) get the matrix c) set inverse matrix d) get the inversed matrix

makeCacheMatrix<-function(x = matrix()) {
	x_inv<-NULL
	set<- function(y) {
		x<<- y
		x_inv<<- NULL
	}
	get<-function() x
	setinverse<-function(inverse) x_inv<<-inverse
	getinverse<-function() x_inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: returns the inverse of a matrix created by makeCacheMatrix:
## If the the cached inverse is available, cacheSolve retrieves it, while if not, it computes, caches and returns it 

cacheSolve<-function(x,...){
	## Return a matrix that is the inverse of 'x'
	x_inv<-x$getinverse()
	if(!is.null(x_inv)){
		message("getting cached data")
		return(x_inv)
	}else{
	data<- x$get()
	x_inv<- solve(data,...)
	x$setinverse(x_inv)
	return(x_inv)
	}
}

