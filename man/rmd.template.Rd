\name{template.files}
\alias{template.files} \alias{word.template} \alias{rmd.template}
\docType{data}
\title{
A R markdown and a Word template file
}
\description{
A R markdown template file that can produce 'MS Word', html, or pdf file based on a flag, and a 'MS Word' template file on which the style of Word output file is based on.
}
\details{
This R markdown template can produce 'MS Word', html, or pdf file based on the flag \code{oFormat} on line 3. The style of 'MS Word' format is based on the 'MS Word' template file. They can be accessed by
\itemize{
    \item \code{system.file("extdata", "rmd.template.Rmd", package = "wfr")}
    \item \code{system.file("extdata", "word.template.for.Rmd.docx", package = "wfr")}
}

Function \code{\link{createRmd}} appends knitr code chunks to this Rmd template to form the final Rmd file.
}
\keyword{template}
\keyword{rmd}
\keyword{MS Word}

