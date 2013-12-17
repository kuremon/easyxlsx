#' @title Retrieve values at LOD in a data frame.
#' @param data the data to apply LOD split on.
#' @param except the index or names of the columns that won't be modified.
#' @param value.only if \code{value.only} is true, only the numeric data frame is returned.
#' Otherwise a list with the numeric data and the indexes is returned.
#' @param transform a function that transform the value in returned data.
#' @export
lodsplit=function(x,values.only=TRUE,pattern="^<",transform=identity){
  UseMethod("lodsplit",x)
}

#' @S3method lodsplit numeric
#' @method lodsplit numeric
lodsplit.numeric=function(x,values.only=TRUE,pattern="^<",transform=identity){
  x
}

#' @S3method lodsplit character
#' @method lodsplit character
lodsplit.character=function(x,values.only=TRUE,pattern="^<",transform=identity){
  n=length(x)
  values=numeric(n)
  
  m=regexec(x,pattern=pattern)
  x.matches=regmatches(x,m,invert=TRUE)
  suppressWarnings(values <- sapply(x.matches,function(el)as.numeric(el[el!=""])))

  lod=sapply(regmatches(x,m),length)==1
  lod[is.na(values)]=NA
  
  values[which(lod)]=transform(values[which(lod)])
  
  if(values.only) return(values)
  list(values=values,lod=lod)
}

#' @S3method lodsplit matrix
#' @method lodsplit matrix
lodsplit.matrix=function(x,values.only=TRUE,pattern="^<",transform=identity){
  dim.x=dim(x)
  L=lodsplit(as.vector(x),values.only,pattern,transform)
  if(values.only) return(L)
  list(values=as.matrix(L$values,dim.x),lod=as.matrix(L$lod,dim.x))
}

#' @S3method lodsplit data.frame
#' @method lodsplit data.frame
lodsplit.data.frame=function(x,values.only=TRUE,pattern="^<",transform=identity){
  #except.index=get_col.index(colnames(x),except)
  idx=sapply(x,is.character)
  
 lodsplit.matrix(as.matrix(x[idx])) 
}


#' @S3method lodsplit list
#' @method lodsplit list
lodsplit.list=function(x,values.only=TRUE,pattern="^<",transform=identity){
  lodsplit(do.call(c,x),values.only,pattern,transform)
}

#   l.split=length(split)
#   if(l.split==0){
#     value=NA
#   }
#   else{
#     value= # to avoid raising a warning with "NA" value.
#   }
#   (l.split>1))
# }
# 
# lodsplit.base.value.only=function(str){
#   lodsplit.base(str)$value  
# }

# lodsplit2=function(data,except=NULL,except.col=except,value.only=TRUE){
#   col.index=get_col.index(colnames(data),except.col)
#   if(value.only){
#     fun=Vectorize(lodsplit.base.value.only)
#   }
#   else{
#     fun=Vectorize(lodsplit.base)
#   }
#   data[col.index]=colwise(fun)(data[col.index])
#   data
# }




# lodsplit.matrix=function(data,,value.only=TRUE,transform=identity){
#   col.index=get_col.index(colnames(data),except)
#   
#   M=as.matrix(data[col.index])
#   
#   split=strsplit(M,split='<')
#   split.value=transform(matrix(sapply(split,function(x)as.numeric(tail(x,1))),dim(M)))
#   
#   data[col.index]=split.value
#   
#   if(value.only) return(data)
#   is.split=matrix(FALSE,nrow(data),ncol(data))
#   is.split[,col.index]=sapply(split,length)>1
#   list(data=data,index=which(is.split,arr.ind=TRUE))
# }


