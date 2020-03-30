#How to add 16 days to each column, stopping at a max

ab <- data.frame(a = 3:30, b = 2:1)
my <- data.frame(2:10)
empty <- data.frame()
na <- NULL

#Given two data frames of different length, return a third array of x amd mx merged by columns
#with NULL filling the empty space 
cbind.pad <- function(x, mx){
  
  
  #Base cases: passing one or two null values, equal dimensions
  if(is.null(x) && is.null(mx))
    return(data.frame())
 
   else if(is.null(x))
    return(mx)
  
  else if(is.null(mx))
    return(x)
  
  len <- max(nrow(x), nrow(mx))
  
  
  if(nrow(x) == nrow(mx))
    return(cbind(x, mx))
  
  #Real work of the method: 
  else if(nrow(x) < nrow(mx)){
    x <- padNULL(x, len)
    return(cbind(x, mx))
    
    
  }
  
  else{
    mx <- padNULL(mx, len)
    return(cbind(x, mx))
  }
  
}

#Recursive function that adds NULLs to the end of data frame x until x reaches the desired len
padNULL <- function(x, len){
  
  if(is_empty(x))
    return(data.frame(rep(NA, len)))
  
  if(nrow(x) == len)
    return(x)
  
  else
    return(padNULL(rbind(x, rep(NA, ncol(x))), len))
  
}


cbind.pad(na, ab)
cbind.pad(ab, na)
cbind.pad(empty, na)
cbind.pad(na, empty)
cbind.pad(na, na)
cbind.pad(na, my)
cbind.pad(empty, empty)
cbind.pad(my, empty)
cbind.pad(empty, my)
