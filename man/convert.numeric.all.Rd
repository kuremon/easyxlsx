\name{convert.numeric.all}
\alias{convert.numeric.all}
\title{Convert cells in a data frame to numeric}
\usage{
  convert.numeric.all(data, except.col = NULL)
}
\arguments{
  \item{data}{the data frame to convert.}

  \item{except.col}{names of the columns to leave
  unchanged. By default the whole array is converted.}
}
\value{
  The modified array.
}
\description{
  Convert cells in a data frame to numeric
}
\seealso{
  \code{\link{apply.to.all}}
}

