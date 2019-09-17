\name{table.cross.ref}
\alias{table.cross.ref} \alias{tCap} \alias{tRef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create the cross-reference string, and the caption of a table with cross-reference
}
\description{
\code{tRef} creates the cross-reference string, and \code{tCap} creates the caption.
}
\usage{
tRef(label, isDocx)
tCap(cap, label, isDocx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{cap}{
        (character) the original caption string
    }
    \item{label}{
        (character) the \code{label} of the table in the knitr code chunk
    }
    \item{isDocx}{
        (boolean) is the output format of the Rmd file \code{word_document2}?
    }
}
\details{
Because the current version of \code{\link[flextable]{flextable}v0.5.5} does not work with \href{https://bookdown.org/yihui/bookdown}{bookdown}v0.12 in automatically producing table cross-reference, these functions are a workaround.
}
\value{
(character) If \code{isDocx} is \code{F}, \code{tRef} returns \code{Table \\@ref(tab:label)} and \code{tCap} simply returns \code{cap}; otherwise, \code{tRef} returns \code{Table. x} and \code{tCap} returns \code{Table. x cap}, where \code{x} is the ordered index of the table.
}
\examples{
library(wfr)
tCap("first table","tab1",FALSE)
tCap("first table","tab1",TRUE)
tCap("second table","tab2",TRUE)
tCap("first table","tab1",TRUE)

tRef("tab2",TRUE)
tRef("tab1",TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rmd }% use one of  RShowDoc("KEYWORDS")
\keyword{ table }% __ONLY ONE__ keyword per line
