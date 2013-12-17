\name{separate_values.units}
\alias{separate_values.units}
\title{Separate parameters names from units}
\usage{
  separate_values.units(x, ...)
}
\arguments{
  \item{x}{the character string to separate}

  \item{...}{Additional arguments to
  \code{\link{data.frame}}}
}
\value{
  A data frame with columns Name and Unit.
}
\description{
  This function allows to separate names from units in the
  same character strings. Typically, when the parameters
  are given in a form name(unit). For example,
  \code{c("L(m)","W(kg)")}.
}
\examples{
\donttest{
x=c("L(m)","W(kg)","Sp (km/h)")
separate_values.units(x)
}
}

