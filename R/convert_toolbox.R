#' @title Convert cells in a data frame to numeric  
#' @param data the data frame to convert.
#' @param except.col names of the columns to leave unchanged. By default the whole array is converted.
#' @return The modified array.
#' @seealso \code{\link{apply.to.all}}
#' @export
convert.numeric.all=function(data,except.col=NULL){
  data=apply.to.all(fun=factor2numeric,data=data,except.col=except.col)
  return(data)
}


#' @title Convert Excel Time to R POSIXct
#' @rdname xlsx2POSIXct
#' @description
#' This an additional \code{as.POSIXct} for atomic vectors of the character subclass \code{xlsx}.  
#' @param x the data to convert.
#' @param tz the time zone. Default is Brisbane Australia.
#' @export
xlsx2POSIXct <- function(x,tz='Australia/Brisbane'){
  x=as.POSIXct((factor2numeric(x)-25569)*86400, tz=tz, origin='1970-01-01')
  return(x)
}

#' @rdname xlsx2POSIXct
#' @export
as.POSIXct.xlsx <- function(x,tz='Australia/Brisbane'){
  x=xlsx2POSIXct(x,tz);
  return(x);
}