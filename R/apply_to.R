#' @title Get the column index
#' @export
get_col.idx=function(colnames,idx){
  if(!is.numeric(except.col)){
    except.col=match(except.col,colnames)
  }
  setdiff(seq(colnames),except.col)
}



#' @title Wrapped function for apply on arrays 
#' @rdname apply.to
#' @param fun the function to apply. 
#' @param data the array (including matrix or data.frame).
#' @param ... optional arguments to \code{fun}.
#' @param except alias for except.row or except.col (see Details). 
#' @param except.row indexes of the rows to leave unchanged.
#' @param except.col names of the columns to leave unchanged.
#' @details
#' By default the function is applied to the whole array.
#' @return The modified array.
#' @seealso \code{\link{apply.to.row}}, \code{\link{apply.to.col}}
apply.to <- function(fun,data,...,to,except.row,except.col){
  row.index=setdiff(seq(nrow(data)),except.row);
  col.index=get_col.index(colnames(data),except.col);
  if(is.null(except.row)){
    data[col.index]=apply(data[col.index],to,fun,...);   
  }
  else{
    warning("If the function is type-coercing, there might be inconsistencies 
            in the resulting data frame as except.row is not NULL.")
    data[row.index,col.index]=apply(data[row.index,col.index,drop=F],to,fun,...);
  }
  return(data);
}

#' @rdname apply.to
#' @export
apply.to.all <- function(fun,data,...,except=NULL,except.row=NULL,except.col=except){
  data=apply.to(fun=fun,data=data,...,to=c(1,2),except.row=except.row,except.col=except.col);
  return(data);
}

#' @rdname apply.to
#' @export
apply.to.row <- function(fun,data,...,except=NULL,except.row=except,except.col=NULL){
  data=apply.to(fun=fun,data=data,...,to=1,except.row=except.row,except.col=except.col);
  return(data);
}

#' @rdname apply.to
#' @export
apply.to.col <- function(fun,data,...,except=NULL,except.row=NULL,except.col=except){
  if(is.null(except.row)){
    data=colwise(fun)(data)
  }
  data=apply.to(fun=fun,data=data,...,to=2,except.row=except.row,except.col=except.col);
  return(data);
}