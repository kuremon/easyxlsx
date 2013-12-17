\name{apply.to}
\alias{apply.to}
\alias{apply.to.all}
\alias{apply.to.col}
\alias{apply.to.row}
\title{Wrapped function for apply on arrays}
\usage{
  apply.to(fun, data, ..., to, except.row, except.col)

  apply.to.all(fun, data, ..., except = NULL,
    except.row = NULL, except.col = except)

  apply.to.row(fun, data, ..., except = NULL,
    except.row = except, except.col = NULL)

  apply.to.col(fun, data, ..., except = NULL,
    except.row = NULL, except.col = except)
}
\arguments{
  \item{fun}{the function to apply.}

  \item{data}{the array (including matrix or data.frame).}

  \item{...}{optional arguments to \code{fun}.}

  \item{except}{alias for except.row or except.col (see
  Details).}

  \item{except.row}{indexes of the rows to leave
  unchanged.}

  \item{except.col}{names of the columns to leave
  unchanged.}
}
\value{
  The modified array.
}
\description{
  Wrapped function for apply on arrays
}
\details{
  By default the function is applied to the whole array.
}
\seealso{
  \code{\link{apply.to.row}}, \code{\link{apply.to.col}}
}

