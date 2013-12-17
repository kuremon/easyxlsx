#' @title Transpose a data.frame
#' @description 
#' This function transposes a data.frame.  
#' @param data data.frame to transpose.
#' @param id index of the column to .
#' @param variable.name the columns names of the returned data frame. By default these names are constructed from 
#' \code{data[[1]][-1]} that is the first column of \code{data} without the header row value.
#' @param id.vars the 
#' @details
#' The levels of factors are updated to include the new value.
#' @return A data frame with the column names \code{variable.name} and values corresponding to the transposed values
#' of \code{data}.
#' @export
transpose.data.frame <- function(data,new.header=.row,current.header.to=1,
                                 new.observation.name="observation",special.letter="V"){
  data=set.row.names(data,new.header);
  data=as.data.frame(t(data));
  names.data=names(data)
  if(contains.integer(names.data)){
    names(data)=paste0(special.letter,names.data);
  }
  
  if(is.null(current.header.to)){
    warning("The current header is not going to appeared is the new data frame.")
    row.names(data)=seq(nrow(data));
  }
  else{
    data=get.row.names(data,position=current.header.to,var.name=new.observation.name);    
  }
  
  return(data);
}