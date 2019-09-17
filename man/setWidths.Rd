\name{setWidths}
\alias{setWidths} \alias{breakRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Set column widths of a table
}
\description{
    Properly setting column widths given the maximum width of the table, for non-html output only.
}
\usage{
setWidths(x, header1, maxTableWidth = 7, rowHeaderInd = NULL,
          minFontSize=9, nRowPerRowHeader=NULL)
breakRatio(aStr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A flextable object
  }
  \item{header1}{
    (character vector) the first row of the column header
  }

  \item{maxTableWidth}{
    (numeric, 7.0) the maximum width of the table in inch. The default 7.0 corresponds to "PAGE LAYOUT" > "size" > "letter"; "Margins" > "Moderate" in MS Word.
  }

  \item{rowHeaderInd}{
    (integer, NULL) See \code{rowHeaderInd} in \code{\link{rmdTable}}. Because row headers are bold font, their lengths in inch is increased by 10\%.
  }

  \item{minFontSize}{
    (integer, 9) The minimum font size in table body.
  }

  \item{nRowPerRowHeader}{
    (integer vector, NULL) The average number of rows spanned by each row header under a row header index. So it's a vector of length \code{rowHeaderInd}.
    }

  \item{aStr}{
    (a character string) The string is the content of a header cell.
  }

}
\details{
    \code{setWidths} sets the width of each column of a table to fit \code{maxTableWidth}. Here is a brief description of its algorithm:

    let \code{HBWidths = mapply(max, wHeader,wBody)}

    \enumerate{
        \item if(sum(HBWidths) <= maxTableWidth), set HBWidths to be the final table widths.
        \item Else if(sum(HBWidths)/maxTableWidth < 1.08), reducing font size by 1, set HBWidths*0.92 to be the final table widths.
        \item Else
            \enumerate{
        	\item For columns where the header is longer than the body and the header is a single word, wrap the header at a non-letter character closest to the middle of the header.
        	\item If the table still doesn't fit, further wrapping the columns of row headers if any, and if the cells under those columns span vertically across multiple cells.
        	\item If the table still doesn't fit, reduce font size up to \code{minFontSize}
        	\item If the table still doesn't fit, wrap the table body, staring from the longest table columns, until the table fits.
        	}
    }

    \code{breakRatio} computes the wrapping point of a header.
}
\value{
    (numeric) by \code{breakRatio}, the ratio of the original length after wrapping.
    (list) by \code{setWidths}. The list contains 1)widths: a numeric vector of column widths and 2)fs: the new font size.
}

\keyword{ width }% use one of  RShowDoc("KEYWORDS")
\keyword{ widths }% __ONLY ONE__ keyword per line
\keyword{ table }% __ONLY ONE__ keyword per line
