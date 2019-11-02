\name{showObj}
\alias{showObj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display an object in knitr code chunk
}
\description{
Display an object in knitr code chunk and set the caption with cross reference.
}
\usage{
showObj(oDf,objID=NULL,isDocx=F,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{oDf}{
        (data.frame or matrix) Refers to \code{\link{OUTPUTS}}, the table containing the information of saved objects.
    }
    \item{isDocx}{
        (boolean, F) is the Rmd output word_document2?
    }
    \item{objID}{
        (character, NULL) The objID of the object in \code{oDf}. If NULL, it is assigned the \code{label} of the code chunk. But when running the code chunk alone, e.g. for debugging, the \code{label} is not accessible and must be explicitly supplied.
    }
    \item{...}{
        parameters passed to \code{\link{rmdTable}} to define table properties. Unsupplied properties are taken from the corresponding columns on the row identified by \code{objID}, if those cells are not \code{NA}.
    }
}
\value{
An object identified by \code{objID}, can be a ggplot2 or a table (flextable if \code{isDocx T}; kable otherwise)
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rmdTable}} \code{\link{OUTPUTS}}
}
\examples{
    library(wfr)
    library(ggplot2)
    isDocx=TRUE

    df1=data.frame(A=c("a","a","b3"),
                   B=c("b1","b2","b3"),
                   C1=1:3,C2=2:4)
    colWidth = "2,1,1,1"
    header = "A | A | C | C || A | A | C1 | C2"
    footer = "A|Arkansas$~ref$|1|header
    || C1|Kansas$^ref$|x|header
    || a|Arizona|2|body"

    \dontrun{
        saveOutput(df1,"t1.csv",caption = "1st testing table",header = header,
                   footer = footer, colWidth = colWidth,fontSize = 12 )
        saveOutput(qplot(1:10,1:10),caption = "1st testing fig")

        OUTPUTS[,"oPath"]=getwd()
        write.csv(OUTPUTS,"all.outputs.csv")

        oDf=read.csv("all.outputs.csv",stringsAsFactors = FALSE)
        showObj(oDf,isDocx,objID = "tab1",rowHeaderInd=2)
        showObj(oDf,objID = "fig2")
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rmd }% use one of  RShowDoc("KEYWORDS")
