\name{str2list}
\alias{str2list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a character string to list of character vectors
}
\description{
In the character string, '|' separates items inside a vector; '||' separates vectors.
}
\usage{
str2list(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{x}{
        (character) a character string
    }
}
\value{
(list of character vectors)
}
\note{
This is an internal function used in \code{\link{rmdTable}} so that \code{header} and \code{footer} can accept character string.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rmdTable}}
}
\examples{
str2list("A | A | C | C || A | A | C1 | C2")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ string }% use one of  RShowDoc("KEYWORDS")
\keyword{ list }% __ONLY ONE__ keyword per line
