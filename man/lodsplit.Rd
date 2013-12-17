\name{lodsplit}
\alias{lodsplit}
\title{Retrieve values at LOD in a data frame.}
\usage{
  lodsplit(x, values.only = TRUE, pattern = "^<",
    transform = identity)
}
\arguments{
  \item{data}{the data to apply LOD split on.}

  \item{except}{the index or names of the columns that
  won't be modified.}

  \item{value.only}{if \code{value.only} is true, only the
  numeric data frame is returned. Otherwise a list with the
  numeric data and the indexes is returned.}

  \item{transform}{a function that transform the value in
  returned data.}
}
\description{
  Retrieve values at LOD in a data frame.
}

