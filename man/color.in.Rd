\name{color.in}
\alias{color.in}
\title{Color spreadsheets cells background}
\usage{
  color.in(x, sheet, row, col, color)
}
\arguments{
  \item{x}{Can be a workbook or a file name. If \code{x} is
  a filename, \code{color.in} opens the corresponding
  workbook, applies the background color to the specified
  cells and saves the result.}

  \item{sheet}{Name or index of the sheet the cells to
  color are on.}

  \item{row}{Row indexes of the cells to apply the coloring
  to.}

  \item{col}{Column indexes of the cells to apply the
  coloring to.}

  \item{color}{Character string of the color to apply. By
  default, \code{color="red"}.}
}
\description{
  Color spreadsheets cells background
}

