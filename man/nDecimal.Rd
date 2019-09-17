\name{nDecimal}
\alias{nDecimal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return the number of decimal points of a number
}
\description{
    Trailing zeros are not counted.
}
\usage{
nDecimal(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
(numeric or integer vector) a number vector
}
}
\value{
(integer vector) numbers of decimal points, excluding trailing zeros, or \code{NA} if \code{x} is \code{NA}
}
\note{
This is an internal function used in \code{\link{num2formattedStr}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{num2formattedStr}}
}
\examples{
library(wfr)
nDecimal(c(1.010,0.3,NA))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ format }% use one of  RShowDoc("KEYWORDS")
\keyword{ number }% __ONLY ONE__ keyword per line
