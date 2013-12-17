\name{readWorkbook}
\alias{readWorkbook}
\title{Read entire Excel workbook}
\usage{
  readWorkbook(filename = file.choose(),
    as.data.frame = TRUE, keep, drop, name.var, position,
    as.factor, verbose = TRUE, ...)
}
\arguments{
  \item{filename}{The name of excel workbook to read. If
  not specified \code{\link{file.choose}} is called.}

  \item{as.data.frame}{Should the data frames read from the
  workbook's spreadsheets be stacked together.}

  \item{keep}{Names (or position) of the spreadsheets to
  keep. If not specified all spreadsheets are read.}

  \item{drop}{Names (or position) of the spreadsheets to
  drop. If not specified all spreadsheets are read.
  \code{keep} has precedence over \code{drop}.}

  \item{name.var}{If \code{name.var} is not missing, the
  name of each spreadsheets is added to the corresponding
  data frames as a new variable with name \code{name.var}
  and position \code{position}.}

  \item{position}{See above. By default,
  \code{position=1}.}

  \item{as.factor}{Should the new variable be a factor? By
  default, \code{as.factor=TRUE}.}

  \item{verbose}{Should messages be displayed?}

  \item{...}{Additional arguments to
  \code{\link{readWorksheet}}}
}
\description{
  Read entire Excel workbook
}

