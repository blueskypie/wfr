\name{createRmd}
\alias{createRmd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a R markdown file
}
\description{
Automatically creates a Rmd file by appending knitr code chunks to an existing \code{\link{rmd.template}}
}
\usage{
createRmd(outputRmdFn, outputListFn, rmdTemplateFn =
          system.file("extdata", "rmd.template.Rmd", package = "wfr"),
          libs = c("ggplot2"), tabPars=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{outputRmdFn}{
        (character) The file name of the produced Rmd file, can contain file paths.
    }

    \item{outputListFn}{
        (character) The file name of the text file containing the table from \code{\link{OUTPUTS}}
    }

    \item{rmdTemplateFn}{
        (character,
        \code{system.file("extdata", "rmd.template.Rmd",package = "wfr")}) The file name of the \code{\link{rmd.template}}
    }

    \item{libs}{
        (character, \code{"ggplot2"}) The names of the libraries needed to execute the object displayed in each knitr code chunk
    }

    \item{tabPars}{
        (character, NULL) Table parameters passed to \code{\link{showObj}}, excluding the 1st three parameters. For example, "fontSize=12, theme='plain'".
    }
}

\details{
    This function creates a Rmd file by appending knitr code chunks to the existing \code{\link{rmd.template}}.

    The first appended code chunk is for setting up: loading libraries in \code{libs} and \code{\link{rmdTable}}, and reading \code{outputListFn} into a data.frame. The following code chunks are one for each row in \code{outputListFn}, ordered by \code{rmdInd} and filtered by \code{eval} in \code{\link{saveOutput}}. \code{\link{showObj}} displays of the object (figure or table) in each row. By default, only its top three parameters are included in the code chunk; additional parameters for table display should be specified through \code{tabPars}.

    Lines containing R markdown section header in their first cell can be inserted into the Excel file pointed by \code{outputListFn}. Here is an example where \code{.} represents empty space and \code{...} the remaining cells.

    \code{  oPath   rdsFileName   oFileName   caption   rmdInd   eval   ...}

	\code{# Method     .              .          .       1       TRUE   .}

	\code{  /path   tab1.rds      tab1.csv   table1      2       TRUE   ...}





    When the Excel file contains many lines, adding those section headers make it easier to see the structure of the R markdown file, and the tables and figures at each layer. \code{createRmd} writes those section headers directly into the produced R markdown file.

    The final Rmd file should be edited, e.g. the \code{isDocx, outputFileName, title, author} and optionally \code{wordTemplateFileName} for a customized Word template, before knitting.
}

\value{
none
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{OUTPUTS}}, \code{\link{rmd.template}}
}
\examples{
    library(wfr)
    library(ggplot2)

    df1=data.frame(A=c("a","a","b3"),
                   B=c("b1","b2","b3"),
                   C1=1:3,C2=2:4)
    \donttest{
        tmpDir = tempdir()
        ofn = file.path(tmpDir, "tab.1.csv")
        saveOutput(df1, oFileName = ofn, caption="this is a testing table")

        ofn = file.path(tmpDir, "fig.1.png")
        saveOutput(qplot(1:10,1:10), oFileName=ofn, caption="this is a testing plot")

        exFn=file.path(tmpDir, "all.outputs.xlsx")
        rmdFn=file.path(tmpDir, "all.outputs.Rmd")
        writeExcel(exFn)
        createRmd(rmdFn,exFn, tabPars="fontSize=12,theme='plain'")
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rmd }% use one of  RShowDoc("KEYWORDS")
\keyword{ template }% __ONLY ONE__ keyword per line
