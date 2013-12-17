#' @title Complete missing vectors
#' @description All values passing a given test are replaced by the first value available before.
#' @param x the vector to modify.
#' @param test the test to identify values to replace. By default NAs values are replaced.
#' @export
#' @examples
#' \donttest{
#' x=c(1,NA,NA,2,3,4,NA,5,NA,NA,NA)
#' complete_missing(x[-1])
#' complete_missing(x)
#' }
complete_missing=function(x,test=is.na){
  int=where.T(do.call(test,list(x)))
  for(k in seq(nrow(int))){
    int.k=int[k,]
    from=int.k$first
    to=int.k$last
    if(from!=1L) x[seq(from,to)]=x[from-1]
  }
  x
}