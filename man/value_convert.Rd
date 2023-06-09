% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{value_convert}
\alias{value_convert}
\title{Simple conversions}
\usage{
value_convert(x, to = default_convert)
}
\arguments{
\item{x}{A vector of character values}

\item{to}{What to convert \code{x} to (see details for more)}
}
\value{
A parsed value from \code{x}
}
\description{
Convert character to data types
}
\details{
\code{to} can be one of several values.  Firstly the default of \code{default}
calls several additional functions that attempt to resolve a transformation
from a \code{character} vector to a different type.  It is recommended for users
to enter their own specifications instead.  Secondly, a \code{function} (with a
single argument) can be passed which will then be applied directly to \code{x}.
Third, a \emph{prototype} value can be passed.  This might be risky for special
types.  Here, the values of \code{\link[=mode]{mode()}}, \code{\link[=storage.mode]{storage.mode()}}, \code{\link[=attributes]{attributes()}}, and
\code{\link[=class]{class()}} are captured and reassigned from \code{to} to \code{x}.  A special check is
implemented for \code{factor}s to more safely convert.  Lastly, \code{NULL} will do
nothing and will simply return \code{x}.
}
\examples{
str(value_convert("2023-03-05", as.Date))
value_convert("a", factor(letters))
}
