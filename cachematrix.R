# R programming course week 3 Assignment
#

# makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.

#Ths fucntions makeChaceMatrix take a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The 
#matrix object can cache its own object.

# new <<- operator assigns a value to an object in an enviroment that is diffrent
#from the current environment

#take matrix as an input 
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        #set the value of the matrix
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        getMatrix <- function() x     #get the value of the Matrix
        setInverse <- function(inverse) invertMatrix <<- inverse #set the value of the invertible matrix
        getInverse <- function() invMatrix                 #get the evalue of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}
#########
# cacheSolve 
# cacheSolve function takes the output of the previous matrix makeCacheMatrix(matrix) as
#an input and check the inverse matrix from makeCacheMatrix(matrix) has any value in it
#or not. In case inverse matrix from makeCacheMatrix(matrix) is emptu, it get the original
#matrix data from and set the invertible matrix by using the solve function.
#In a case inverse matrix from makeCacheMatrix(matrix) has some value in it (always works
#after funning the code 1st time), it returns the message "Getting cached invertible Matrix"
#adn the cached object.

cacheSolve <- function(x, ...) {
        #get the value of the invertible matrix from the makeCacheMatrix function
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {   #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix") #type msg: "Getting cached invertible matrix
                return(invMatrix) # return the invertible matrix
        }
        #if value of the invertible matrix is NULL then"
        MatrixData <- x$getMatrix()     #get the ortiginal matrix data
        invMatrix <- solve(MatrixData, ...) #use solve function to inverse the matrix
        x$setInverse(invMatrix)    #set the ivertible matrix
        return(invMatrix)       #return the invertible matrix
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get the value of the invertible matrix from the makeCacheMatrix function
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
                return(invMatrix)                             #return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         #set the invertible matrix 
        return(invMatrix)                               #return the invertible matrix
}


##### testing

my_matrix <- matrix(1:4, 2, 2)
my_matrix

cacheMatrix <- makeCacheMatrix(my_matrix)
cacheMatrix$getMatrix()
cacheMatrix$getInverse()

cacheSolve(cacheMatrix)

my_matrix <- matrix(c(3,1,39,5), 2, 2)
my_matrix

cacheMatrix <- makeCacheMatrix(my_matrix)
cacheMatrix$getMatrix()
cacheMatrix$getInverse()

cacheSolve(cacheMatrix)











