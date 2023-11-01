\name{rmdTable}
\alias{myFlexTable} \alias{myKable} \alias{rmdTable} \alias{setFlexTableFontSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a table
}
\description{
    Construct a table in html, pdf, or word document, format the numeric columns, and automatically setting column widths.
}
\usage{
rmdTable(dataDf, header = list(colnames(dataDf)), footer = NULL,
         colWidths = NULL, fontSize = 11, caption = NULL,
         rowHeaderInd = NULL, isDocx = TRUE, nRowScroll = 20,
         nRowDisplay = 200, maxTableWidth = 7,
         theme = c("zebra","box","booktabs","vanilla","tron","vader"),
         char2space=NULL, splitCamelCase=FALSE,
         footerFontSize=9, minFontSize=9,...)

myFlexTable(dataDf,header=list(colnames(dataDf)), footer = NULL,
            colWidths = NULL, fontSize = 11, caption = NULL,
            rowHeaderInd = NULL, mergeBodyColumn = TRUE, maxTableWidth = 7,
            theme = c("zebra","box","booktabs","vanilla","tron","vader"),
            char2space=NULL, splitCamelCase=FALSE,
            footerFontSize=9, minFontSize=9,...)

myKable(dataDf,header = list(colnames(dataDf)), footer = NULL,
        caption = NULL, rowHeaderInd = NULL, nRowScroll = 20,
        theme = c("zebra","box","vanilla"),...)

setFlexTableFontSize(ft,fontSize,footerFontSize=9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{dataDf}{
        (data.frame or matrix) the content of the table to be displayed
    }

    \item{header}{
        (character or list of character vectors, list(colnames(dataDf)))
        \itemize{
            \item list: each vector is the column title. The last vector replaces the \code{colnames(dataDf)}. Neighboring cells of identical content in the header will be merged into one cell. For \code{myKable}, merge is only horizontal and not on the last row.
            \item character: use '|' to separate cell, and '||' to separate rows. It is converted to list using \link{str2list}
        }
    }

    \item{footer}{
        (character or list of character vectors, NULL)
        \itemize{
            \item list: each vector contains the following items:
                \enumerate{
                    \item (optional, character). The cell content which the footnote refers to.
                    \item (character). The content of the footer. use \code{$~i$} and \code{$^i$} to represent the sub/super-script of \code{i}
                    \item (optional, character). The super-script of #1
                    \item (optional, character, "header" or "body") the portion of the table where #1 is to be searched. If "body", only the columns in \code{rowHeaderInd} are searched.
                }
            \item character: use '|' to separate cell, and '||' to separate rows. It is converted to list using \code{\link{str2list}}
        }
    }

    \item{colWidths}{
        (character or numeric vector, NULL). For \code{myFlexTable} only, the column widths, a numerical vector of the length of \code{ncol(dataDf)}. Unit is inch. It can also be a character string where the numbers are separated by ',', e.g. "2,1,1,1".
    }

    \item{fontSize}{
        (integer, 10) For \code{myFlexTable} only, the font size of the header and body. Font size of footer is \code{fontSize - 2}.
    }

    \item{caption}{
        (character, NULL) the caption of the table.
    }

    \item{rowHeaderInd}{
        (integer, NULL). Row headers are the columns in the left of table body serving as headers for rows in the table body. \code{rowHeaderInd} are the last index of those columns, so the column indices of row headers is \code{1:rowHeaderInd}. If \code{rowHeaderInd} is specified, the font of row header becomes bold, and neighboring cells of identical content are collapsed. The merge can be both horizontal and vertical in \code{myFlexTable}, and only vertical in \code{myKable}.
    }

    \item{mergeBodyColumn}{
        (boolean, TRUE) \code{if(mergeBodyColumn && rowHeaderInd > 1)}, neighboring horizontal cells in table body are merged if they are in rows where exists identical neighboring horizontal cells in row header. This flag is to prevent, if set to \code{FALSE}, the merging of identical neighboring horizontal cells in table body, when the intention is to limit such merging to row header only.
    }

    \item{isDocx}{
        (boolean, TRUE) if \code{TRUE}, use \code{myFlexTable}; otherwise, \code{myKable}
    }

    \item{nRowScroll}{
        (integer, 20) For \code{myKable} only, the cutoff on number of rows to apply a scroll window.
    }

    \item{nRowDisplay}{
        (integer, 200) For \code{myFlexTable} only, the cutoff on number of rows to display. If there are more than \code{nRowDisplay} rows in the table, the caption of the table is appended "(top \code{nRowDisplay} rows only)".
    }

    \item{theme}{
        (character) The theme of the table.
    }

    \item{maxTableWidth,minFontSize}{
        see those parameters in \code{\link{setWidths}}. For \code{myFlexTable} only.
    }

    \item{char2space}{
    (character string, NULL) A regular expression. Should the characters represented by this regular expression in the bottom row of column header be changed to space? If so, when the column header is wrapped, the wrapping happens at a space in stead of the middle of a word. For example, setting \code{char2space = '[^A-z0-9]'} changes all non-letter and non-digit characters into space.
  }

  \item{splitCamelCase}{
    (boolean, FALSE) Should the camel cases in the 1st row of column header be split into separated words? for example, change "youMadeItLOL" into "you Made It LOL". If so, when a column header in camel case is wrapped, the wrapping happens at a space in stead of the middle of a word.
  }

    \item{footerFontSize}{
        (integer,9) For \code{myFlexTable} only; the size of footer font.
    }

    \item{ft}{
        a flextable object
    }

    \item{...}{
        if \code{isDocx}, passed to \code{flextable::flextable()}, and \code{kableExtra::kable()} otherwise.
    }
}

\details{
\code{myFlexTable} and \code{myKable} are wrapper functions of \code{\link[flextable]{flextable}} and \code{\link[knitr]{kable}}, and \code{rmdTable} is a wrapper of the two wrappers with \code{isDocx TRUE} referring to \code{myFlexTable} and \code{FALSE} to \code{myKable}
}
\value{
(flextable or kable object)
}

\note{
Numeric columns are formatted using \code{\link{num2formattedStr}}. If a numeric column is not formatted in the displayed table, probably it is because its data type in \code{dataDf} is not \code{numeric} or \code{integer}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{num2formattedStr}}, \code{\link[flextable]{flextable}}, \code{\link[knitr]{kable}}
}

\examples{
library(wfr)
df1=data.frame(A=c("a","a","b3"),
               B=c("b1","b2","b3"),
               C1=c(1001.123,58.04,32.01),
               C2=c(-0.00321, 0.0121, 0.325))

header=list(c('A','A','C','C'),
            c('A','A','C1','C2'))

footer=list(c("A","Arkansas$~ref$",'1','header'),
            c("C1","Kansas$^ref$",'x','header'),
            c('a',"Arizona",'2','body'))

rmdTable(df1,header = header,
           rowHeaderInd = 2,
           footer = footer,
           caption = "my first table",
           colWidths = c(2,1,1,1),
           fontSize = 12, isDocx = TRUE)

colWidths = "2,1,1,1"
header = "A | A | C | C || A | A | C1 | C2"
footer = "A|Arkansas$~ref$|1|header
               || C1|Kansas$^ref$|x|header
               || a|Arizona|2|body"

rmdTable(df1,header = header,
           rowHeaderInd = 2,
           footer = footer,
           caption = "my second table",
           colWidths = colWidths,
           fontSize = 12, isDocx = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }% use one of  RShowDoc("KEYWORDS")
\keyword{ rmd }% __ONLY ONE__ keyword per line
