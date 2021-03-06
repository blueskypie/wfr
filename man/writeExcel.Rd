\name{writeExcel}
\alias{writeExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Save the \code{\link{OUTPUTS}} to an Excel file
}
\description{
    While writing the \code{\link{OUTPUTS}} to an Excel file, column \code{oFileName} is embedded with
    URL \code{file.path(df1[,"oPath"],df1[,"oFileName"])}.
}
\usage{
writeExcel(fileName, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{fileName}{
        (character) The name of the Excel file with extension 'xlsx'. Can contain relative or absolute file paths.
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

  \donttest{
    tmpDir = tempdir()
    ofn = file.path(tmpDir, "tab.1.csv")
    saveOutput(df1,ofn,caption = "1st testing table",header = header,
               footer = footer,colWidth = colWidth,fontSize = 12 )

    saveOutput(qplot(1:10,1:10),oPath=tmpDir,caption = "1st testing fig")

    fn1=file.path(tmpDir, "output.xlsx")
    writeExcel(fn1)
    df2=read.xlsx(fn1)
  }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ excel }% use one of  RShowDoc("KEYWORDS")
