\name{num2formattedStr}
\alias{num2formattedStr} \alias{isVecNumeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format a vector of numbers based on their distribution
}
\description{
    Format a vector of numbers based on their distribution, removing non-informative digits.
}
\usage{
isVecNumeric(v)
num2formattedStr(v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
        (vector) a vector
    }
}
\details{
\code{isVecNumeric} determines whether a vector, numeric or not, contains numbers only. This function is used because often numeric columns are accidently formatted as character or factor classes. If TRUE from \code{isVecNumeric}, \code{num2formattedStr} makes decision on the following four aspects of formatting based on the min, median, max values of the vector, so that the formatted numbers carry enough information and are in a length less than 10 characters.
\itemize{
    \item number of significant digits
    \item number of decimal points
    \item whether to apply 1000 separator ','
    \item whether to apply scientific notation
    }
}
\value{
(boolean) by \code{isVecNumeric}

(character vector) formatted numbers in character by \code{num2formattedStr}
}
\examples{
library(wfr)
v1=c(-1032.789, 389.4789, 78.00)
num2formattedStr(v1)
v1=c(3.00, 8.00, -10.000)
num2formattedStr(v1)
v1=c(-0.1289, 0.0489, 0.0003765)
num2formattedStr(v1)
v1=c(-0.1289, 0.0489, 0.03765, NA, Inf, -Inf, NaN)
num2formattedStr(v1)
v1=c(-0.1289, 0.0489, 0.03765, NA,'', Inf, -Inf, NaN)
class(v1)
num2formattedStr(v1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ format }% use one of  RShowDoc("KEYWORDS")
\keyword{ number }% __ONLY ONE__ keyword per line
