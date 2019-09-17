\name{loadLibs}
\alias{loadLibs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load libraries into working space
}
\description{
    It loads libraries successfully in certain scenario where \code{require/library} fails.
}
\usage{
loadLibs(failedPackages)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{failedPackages}{
        (character vector) names of libraries to load
    }
}
\details{
This function loads libraries into the working space. It iterates over each path in \code{.libPaths()} using \code{require} until a library is loaded successfully. In comparison, \code{require(x)} for a library \code{x} fails if \code{x} is present in multiple search paths but the 1st presence causes loading error, e.g. due to dependency or other issues.

It raises error if not all libraries are loaded successfully.
}
\value{
none
}
\examples{
library(wfr)
loadLibs(c("ggplot2","flextable"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ library }% use one of  RShowDoc("KEYWORDS")
