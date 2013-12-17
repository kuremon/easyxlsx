#' @title Write a list of data frames into an Excel workbook 
#' @param L The list of data frames to write.
#' @param filename The name of the output file.
#' @param ... Additional arguments to \code{\link{writeWorksheet}}
#' @export
writeWorkbook=function(L,filename,...){
  sheets=names(L)
  wb=loadWorkbook(filename,create=TRUE)
  for(k in seq(L)){
    createSheet(wb,sheets[k])
    writeWorksheet(wb, L[[k]], sheet = sheets[k], ...)
  }
  saveWorkbook(wb)
}