\name{setHtmlHeaderProperty}
\alias{setHtmlHeaderProperty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the formatting properties of Title, Author, and Date in Rmd file
}
\description{
Font size in px, font family, and color can be set. Alignment is center.
}
\usage{
setHtmlHeaderProperty(
    titleFontSize = NULL, titleFontFamily = NULL, titleColor = NULL,
    authorFontSize = NULL, authorFontFamily = NULL, authorColor = NULL,
    dateFontSize = NULL, dateFontFamily = NULL, dateColor = NULL,
    pageWidth=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{titleFontSize}{
%%     ~~Describe \code{titleFontSize} here~~
}
  \item{titleFontFamily}{
%%     ~~Describe \code{titleFontFamily} here~~
}
  \item{titleColor}{
%%     ~~Describe \code{titleColor} here~~
}
  \item{authorFontSize}{
%%     ~~Describe \code{authorFontSize} here~~
}
  \item{authorFontFamily}{
%%     ~~Describe \code{authorFontFamily} here~~
}
  \item{authorColor}{
%%     ~~Describe \code{authorColor} here~~
}
  \item{dateFontSize}{
%%     ~~Describe \code{dateFontSize} here~~
}
  \item{dateFontFamily}{
%%     ~~Describe \code{dateFontFamily} here~~
}
  \item{dateColor}{
%%     ~~Describe \code{dateColor} here~~
}
  \item{pageWidth}{
        (numeric) the page width in px
}
}
\references{
\href{https://stackoverflow.com/questions/43404487/how-do-i-center-my-yaml-heading-in-an-r-markdown-html-document}{stackoverflow link}
}
\examples{
setHtmlHeaderProperty(titleFontSize=18,
    titleFontFamily='"Times New Roman", Times, serif',
    titleColor='DarkBlue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ html }% use one of  RShowDoc("KEYWORDS")
