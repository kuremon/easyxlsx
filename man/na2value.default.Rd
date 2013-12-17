\name{na2value.default}
\alias{na2value.default}
\title{Change NA to a specified value}
\usage{
  na2value.default(data, value)
}
\arguments{
  \item{data}{vector or list to modify.}

  \item{value}{the value to replace NA.}
}
\description{
  This the default function for S3 method na2value.
}
\details{
  The levels of factors are updated to include the new
  value.
}

