\name{transpose.data.frame}
\alias{transpose.data.frame}
\title{Transpose a data.frame}
\usage{
  transpose.data.frame(data, new.header = .row,
    current.header.to = 1,
    new.observation.name = "observation",
    special.letter = "V")
}
\arguments{
  \item{data}{data.frame to transpose.}

  \item{id}{index of the column to .}

  \item{variable.name}{the columns names of the returned
  data frame. By default these names are constructed from
  \code{data[[1]][-1]} that is the first column of
  \code{data} without the header row value.}

  \item{id.vars}{the}
}
\value{
  A data frame with the column names \code{variable.name}
  and values corresponding to the transposed values of
  \code{data}.
}
\description{
  This function transposes a data.frame.
}
\details{
  The levels of factors are updated to include the new
  value.
}

