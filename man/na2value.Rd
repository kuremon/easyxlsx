\name{na2value}
\alias{na2value}
\title{Change NA to a specified value}
\usage{
  na2value(data, value)
}
\arguments{
  \item{data}{data to modify (can be a vector or a data
  frame).}

  \item{value}{the value to replace NA.}
}
\description{
  This the generic na2value function. See the following
  functions for the details about different data
  structures.
}
\details{
  \itemize{ \item \code{\link{na2value.default}} when
  \code{data} is not a data.frame.  \item
  \code{\link{na2value.data.frame}} when \code{data} is a
  data frame. } In both cases, the levels of factors are
  updated to include the new value.
}

