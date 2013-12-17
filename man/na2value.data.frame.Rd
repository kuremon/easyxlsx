\name{na2value.data.frame}
\alias{na2value.data.frame}
\title{Change NA to a specified value}
\usage{
  na2value.data.frame(data, value)
}
\arguments{
  \item{data}{data frame to modify.}

  \item{value}{the value to replace NA.}
}
\description{
  This is the na2value function used when the data supplied
  is a data frame.
}
\details{
  The function applies \code{\link{na2value.default}} to
  all columns of the data frame in order to keep consistent
  levels in factors.
}

