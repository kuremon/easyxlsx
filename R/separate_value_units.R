#' @title Separate parameters names from units
#' @description
#' This function allows to separate names from units in the same character strings. Typically, when
#' the parameters are given in a form name(unit). For example, \code{c("L(m)","W(kg)")}.
#' @param x the character string to separate
#' @param ... Additional arguments to \code{\link{data.frame}}
#' @return A data frame with columns Name and Unit.
#' @export
#' @examples
#' \donttest{
#' x=c("L(m)","W(kg)","Sp (km/h)")
#' separate_values.units(x)
#' }
separate_values.units=function(x,...){
  idx=regexec("\\(\u03bc*[[:alpha:]]+/*[[:alpha:]]*\\)$|\\(\\%\\)",x)
  units=regmatches(x,idx)
  units=do.call(c,lapply(units,function(str)ifelse(length(str)==0,"no unit",str)))
  units=gsub("\\(|\\)","",units)
  names=gsub(" $","",do.call(c,lapply(regmatches(x,idx,invert=TRUE),function(l)l[[1]])))
  data.frame(Name=names,Unit=units,...)
}