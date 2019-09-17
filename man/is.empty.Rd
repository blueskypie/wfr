\name{is.empty}
\alias{is.empty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine if an object is empty
}

\description{
    Determine if an object is zero length list or factor, or a vector full of NULL, space, dot,NA, Inf, or NaN. It returns a \strong{single} boolean value.
}

\usage{
is.empty(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{x}{
        (any object)
    }
}

\value{
(boolean) a single value
}

\examples{
is.empty(c(' ','.'))
is.empty(list(NULL))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ empty }% use one of  RShowDoc("KEYWORDS")
