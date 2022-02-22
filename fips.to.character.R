#' Convert numeric FIPS Code to character
#' 
#' Converts a vector of numeric FIPS codes back to character FIPS codes 
#' with appropriate number of zeros.  Returns a character vector.
#'
#' @param x A vector of FIPS codes in numeric format
#' 
#' 


fips.to.character <- function(x){
  
  
  #Determine max length (assumes that at least one of the fips codes is full length)
  num.char<-max(nchar(x))
  
  #Determine number of zeros to add
  num.zeros<-num.char-nchar(x)
  
  #Make vector of zeros
  zero.vec<-base::lapply(num.zeros, FUN= function(y) rep("0",y))
  
  for(i in 1:length(x)){
    y<-zero.vec[[i]]
    if(length(y)!=0){
    x[i]<-paste0(paste0(y,collapse = ""),x[i])
    }
  }

  return(x)
}#END Function
  
#---- END ----
  
  

