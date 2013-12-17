#' @title Change NA to a specified value
#' @description 
#' This the generic na2value function. See the following functions for the details about different data structures.  
#' @param data data to modify (can be a vector or a data frame).
#' @param value the value to replace NA.
#' @details
#' \itemize{
#'   \item \code{\link{na2value.default}} when \code{data} is not a data.frame.
#'   \item \code{\link{na2value.data.frame}} when \code{data} is a data frame.
#' }
#' In both cases, the levels of factors are updated to include the new value. 
na2value <- function(data,value){
  UseMethod("na2value",data);
}

#' @title Change NA to a specified value
#' @description 
#' This the default function for S3 method na2value.  
#' @param data vector or list to modify.
#' @param value the value to replace NA.
#' @details
#' The levels of factors are updated to include the new value.
na2value.default <- function(data,value){
  w=which(is.na(data));
  if(length(w)!=0){
    if(is.factor(data)){
      levels(data)=c(levels(data),value);
    }
    data[w]=value;
  }
  return(data);
}

#' @title Change NA to a specified value
#' @description 
#' This is the na2value function used when the data supplied is a data frame.  
#' @param data data frame to modify.
#' @param value the value to replace NA.
#' @details
#' The function applies \code{\link{na2value.default}} to all columns of the data frame in order to keep consistent levels
#' in factors.
na2value.data.frame=function(data,value){
  data=colwise(na2value,value=value)(data);
  return(data);
}