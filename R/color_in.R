#' @title Color spreadsheets cells background
#' @param x Can be a workbook or a file name. If \code{x} is a filename, \code{color.in} opens the corresponding workbook,
#' applies the background color to the specified cells and saves the result.
#' @param sheet Name or index of the sheet the cells to color are on.
#' @param row Row indexes of the cells to apply the coloring to.
#' @param col Column indexes of the cells to apply the coloring to.
#' @param color Character string of the color to apply. By default, \code{color="red"}.
#' @export
color.in=function(x,sheet,row,col,color){
  UseMethod("color.in",x)
}

#' @S3method color.in workbook
#' @method color.in workbook
color.in.workbook=function(x,sheet,row,col,color="red"){
  cs=XLConnect::createCellStyle(x)
  XLConnect::setFillPattern(cs,fill = XLC$"FILL.SOLID_FOREGROUND")
  XLConnect::setFillForegroundColor(cs, color = XLC[[paste0("COLOR.",toupper(color))]])
  XLConnect::setCellStyle(x, sheet =sheet, row =1+row, col = col, cellstyle = cs)
}

#' @S3method color.in workbook
#' @method color.in workbook
color.in.character=function(x,sheet,row,col,color="red"){
  wb=XLConnect::loadWorkbook(x)
  color.in.workbook(wb,filename,sheet,row,col,color)
  XLConnect::saveWorkbook(wb)
}