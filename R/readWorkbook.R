#' @title Read entire Excel workbook 
#' @param filename The name of excel workbook to read. If not specified \code{\link{file.choose}} is called.
#' @param as.data.frame Should the data frames read from the workbook's spreadsheets be stacked together.
#' @param keep Names (or position) of the spreadsheets to keep. If not specified all spreadsheets are read.
#' @param drop Names (or position) of the spreadsheets to drop. If not specified all spreadsheets are read.
#' \code{keep} has precedence over \code{drop}.
#' @param name.var If \code{name.var} is not missing, the name of each spreadsheets is added to the corresponding
#' data frames as a new variable with name \code{name.var} and position \code{position}.
#' @param position See above. By default, \code{position=1}.
#' @param as.factor Should the new variable be a factor? By default, \code{as.factor=TRUE}.
#' @param verbose Should messages be displayed?
#' @param ... Additional arguments to \code{\link{readWorksheet}}
#' @export
readWorkbook=function(filename=file.choose(),as.data.frame=TRUE,keep,drop,name.var,position,as.factor,verbose=TRUE,...){
    wb=loadWorkbook(filename)
    if(!missing(keep)){
      ws=keep
    }else{
      if(missing(drop)) drop=NULL
      ws=setdiff(getSheets(wb),drop)
    }
    
    L=do.call(readWorksheet,list(wb,ws,...))
    if(!as.data.frame){
      if(verbose) message("A list of data.frame is returned.")
      return(L)
    }else{
      df=ldply(L,identity)
      if(missing(name.var)){
        if(verbose) message("A data.frame is returned.")
        return(df[-1])
      }else{
        if(missing(position)) position=1
        if(missing(as.factor)) as.factor=TRUE
        
        if(as.factor) df[[".id"]]=factor(df[[".id"]]) 
        df=rearrange(df,c(".id"=position))
        df=rename(df,c(".id"=name.var))
        if(verbose) message("A data.frame is returned. The new variable is a ",
                ifelse(as.factor,"factor","character vector")," in position ",position," with name ",name.var,".")
        return(df)
      }
      
    }
}