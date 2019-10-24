\name{saveWfrInfo}
\alias{saveWfrInfo} \alias{restoreWfrInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
save and read back the values of OFCOUNTER and OUTPUTS
}
\description{
these functions are used in R scripts that are separate and run in order, using the SAME \code{\link{OFCOUNTER}} and \code{\link{OUTPUTS}}
}
\usage{
saveWfrInfo(rdsFileName)
restoreWfrInfo(rdsFileName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rdsFileName}{
The name of the rds file keeping the values of \code{\link{OFCOUNTER}} and \code{\link{OUTPUTS}}
}
}
\details{
The rds file is a \code{list(ofc=OFCOUNTER,outp=OUTPUTS)}
}
\value{
(list) by \code{restoreWfrInfo}
}

\seealso{
\code{\link{OFCOUNTER}} and \code{\link{OUTPUTS}}
}
