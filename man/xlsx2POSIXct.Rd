\name{xlsx2POSIXct}
\alias{as.POSIXct.xlsx}
\alias{xlsx2POSIXct}
\title{Convert Excel Time to R POSIXct}
\usage{
  xlsx2POSIXct(x, tz = "Australia/Brisbane")

  as.POSIXct.xlsx(x, tz = "Australia/Brisbane")
}
\arguments{
  \item{x}{the data to convert.}

  \item{tz}{the time zone. Default is Brisbane Australia.}
}
\description{
  This an additional \code{as.POSIXct} for atomic vectors
  of the character subclass \code{xlsx}.
}

