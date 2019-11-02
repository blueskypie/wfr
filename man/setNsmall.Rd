\name{setNsmall}
\alias{setNsmall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the nSmall of a number, and convert it to character
}
\description{
set the nsmall, i.e. number of decimal points, of a number, and convert it to character;  1000 separator ',' is added if \code{max(abs(v)) > 999}.
}
\usage{
setNsmall(v, nSmall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{v}{
        (numeric or integer vector) a number vector
    }

    \item{nSmall}{
        (integer) nsmall, i.e. number of decimal points, of a number (0 <= nsmall <= 20)
    }
}
\value{
(character vector) formatted numbers in character
}
\note{
This is an internal function used in \code{\link{num2formattedStr}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base]{format}}, \code{\link{num2formattedStr}}
}
\examples{
setNsmall(c(1.003,2.1),2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ format }% use one of  RShowDoc("KEYWORDS")
\keyword{ number }% __ONLY ONE__ keyword per line
