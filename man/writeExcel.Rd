\name{writeExcel}
\alias{writeExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Save the \code{\link{OUTPUTS}} to an Excel file
}
\description{
    While writing the \code{\link{OUTPUTS}} to an Excel file, column \code{ofName} is embedded with
    URL \code{file.path(df1[,"oPath"],df1[,"ofName"])}.
}
\usage{
writeExcel(df1, fileName, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{df1}{
        (data frame or matrix) Used to hold the value of \code{\link{OUTPUTS}}
    }
    \item{fileName}{
        (character) The name of the Excel file with extension 'xlsx'
    }
    \item{...}{
        parameters passed to \code{\link[openxlsx]{write.xlsx}}
    }
}
\details{
It is not a general function to save a data.frame or matrix to an Excel file. And there is no \code{append} mode; the target file will be covered if already present.
}
\value{
none
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[openxlsx]{write.xlsx}}
}
\examples{
  library(wfr)
  library(openxlsx)
  library(ggplot2)
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
               footer = footer,colWidth = colWidth,fontSize = 12 )
    saveOutput(qplot(1:10,1:10),caption = "1st testing fig")
    saveOutput(df1,"t1.csv",caption = "1st testing table",header = header,
               footer = footer,colWidth = colWidth,fontSize = 12 )

    OUTPUTS[,"oPath"]=getwd()
    fn1="output.xlsx"
    writeExcel(OUTPUTS,fn1)
    df2=read.xlsx(fn1)
  }
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ excel }% use one of  RShowDoc("KEYWORDS")
