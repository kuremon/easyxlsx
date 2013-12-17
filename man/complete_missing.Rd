\name{complete_missing}
\alias{complete_missing}
\title{Complete missing vectors}
\usage{
  complete_missing(x, test = is.na)
}
\arguments{
  \item{x}{the vector to modify.}

  \item{test}{the test to identify values to replace. By
  default NAs values are replaced.}
}
\description{
  All values passing a given test are replaced by the first
  value available before.
}
\examples{
\donttest{
x=c(1,NA,NA,2,3,4,NA,5,NA,NA,NA)
complete_missing(x[-1])
complete_missing(x)
}
}

