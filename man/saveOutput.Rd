\name{saveOutput}
\alias{saveOutput} \alias{OFCOUNTER} \alias{OUTPUTS} \alias{Global.Variable} \alias{ofcmm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Save the current R image or the object rds file and add its information to the matrix OUTPUTS
}
\description{
Save the current R image or the object rds file and optionally other formats of the object (e.g. csv for tables and png for figures), its caption, and other information related to its appearance in the R markdown file, to the matrix OUTPUTS
}
\usage{
saveOutput(obj=NULL,oFileName, saveWorkspace=FALSE, oPath=getwd(), caption=NA ,
           rmdInd=NA, eval=TRUE,objID=NA, header=NA, footer=NA,
           rowHeaderInd=NA, colWidths=NA, fontSize=11, nRowScroll = 20,
           nRowDisplay = 200, maxTableWidth = 7.2, theme = "zebra",
           numberOutputFiles=TRUE,...)

ofcmm(num=1)
}




\arguments{
   \item{obj}{
    The target object, usually a data.frame or ggplot object. If \code{NULL}, \code{oFileName} can be used to insert an existing figure file into \code{OUTPUTS}, so that the figure file can be auto-included into the Rmd file produced by \code{\link{createRmd}} function.
  }

  \item{oFileName}{
    (character). The file name of the text (e.g. csv) or image (e.g. png) file of the \code{obj} to save. Can contain relative or absolute file paths; Use, e.g. \code{./path1/my.csv}, instead of \code{path1/my.csv} to include a relative path into the file name. If \code{oFileName} contains either a relative or absolute path, the \code{oPath} will be replaced by the whole path.
  }

  \item{saveWorkspace}{
    (boolean, FALSE)
    \itemize{
        \item FALSE: Save the rds file of the \code{obj} only. If \code{oFileName} is provided, the rds file name is \code{paste0(ofNamePrefix,'.rds')}; otherwise,

        \code{paste0(sprintf} \code{("\%03d", OFCOUNTER),'.rds')}.
        \item TRUE: In addition to the rds file, save the R image of the workspace where \code{obj} is generated as \code{paste0(sprintf} \code{("\%03d", OFCOUNTER),'.r.image.rdata')}
    }
  }


  \item{oPath}{
    (character, getwd()). The output path.
  }

  \item{caption}{
    (character, NA). The caption of the target object.
  }

  \item{rmdInd}{
    (integer, \code{\link{OFCOUNTER}}). The order to display \code{obj} in the Rmd file.
  }

  \item{eval}{
    (boolean, TRUE). Should the \code{obj} be included/evaluated in the Rmd file?
  }

  \item{objID}{
    (character, NA, or \code{paste0('tab',OFCOUNTER)}, or \code{paste0('fig',OFCOUNTER)}). The \code{label} of the \code{obj} in the Rmd code chunk. Its default value depends on the data type of \code{obj}: \code{paste0('tab',OFCOUNTER)} for data.frame or matrix, \code{paste0('fig',OFCOUNTER)} for ggplot, and \code{NA} otherwise. If provided, it is cleaned by \code{gsub("[^A-Za-z0-9]","",objID)}, and prefixed by \code{tab} or \code{fig} depending on the type of \code{obj}. If it's already present in \code{OUTPUTS[,"objID"]}, \code{OFCOUNTER} is appended, i.e. \code{objID = paste0(objID,OFCOUNTER)}, to make it unique.
  }

  \item{header}{
    (character, NA). The header of the \code{obj}, if it is displayed as a table in the Rmd file. See parameter \code{header} of \code{\link{rmdTable}} for more details and its character representation.
  }

  \item{footer}{
    (character, NA). The footer of the \code{obj}, if it is displayed as a table in the Rmd file. See parameter \code{footer} of \code{\link{rmdTable}} for more details and its character representation.
  }

  \item{colWidths}{
    (character, NA). The column width of the \code{obj}, if it is displayed as a table in the Rmd file. See parameter \code{colWidths} of \code{\link{rmdTable}} for more details and its character representation.
  }

  \item{rowHeaderInd, fontSize, nRowScroll, nRowDisplay, maxTableWidth, theme}{
    Parameters for \code{obj}, if it is displayed as a table in the Rmd file. See same parameters in \code{\link{rmdTable}} for more details.
  }

  \item{numberOutputFiles}{
    (boolean, TRUE). Should the output files be numbered?
  }

  \item{...}{
    parameters passed to \code{\link[utils]{write.csv}} or \code{\link[ggplot2]{ggsave}} to save the \code{obj}, if a data.frame or matrix, as text file.
  }

   \item{num}{
     the number to deduct from \code{OFCOUNTER}
   }
}


\details{
For the simplicity of coding, two global variables are created for this function:
\itemize{
    \item{OFCOUNTER}{
    (integer,1). A global variable to count the number of outputs, initial value is 1.
  }

    \item{OUTPUTS}{
    (character matrix, NULL). A global variable to record the information of the current r image file and \code{obj}. At the 1st run of \code{saveOutput}, it is assigned to be a character matrix of following columns:
    \itemize{
        \item \code{rImageName} The name of saved R image or rds file, depending on the flag \code{saveWorkspace}.
        \item all other parameters of \code{saveOutput} except \code{obj}, and values assigned by them.
    }
  }
}

\code{saveOutput} does the following:
    \enumerate{
        \item Save the current R image as \code{paste0(sprintf("\%03d", OFCOUNTER),'.r.image.rdata')} or the object rds file in the \code{oPath} directory.
        \item If \code{oFileName} is provided, save \code{obj} as \code{paste0(sprintf("\%03d", OFCOUNTER),'.',oFileName)} in the \code{oPath} directory, using either \code{utils::write.csv} or \code{ggplot2::ggsave} depending on the data type of \code{obj}
        \item Create the matrix \code{OUTPUTS} if it is NULL, and assign the values of all other parameters to corresponding columns in \code{OUTPUTS}
        \item Increment \code{\link{OFCOUNTER}} by 1
    }

\code{ofcmm} deducts \code{OFCOUNTER} by \code{num}. It can be used to remove the information appended to \code{OUTPUTS} from running \code{saveOutput}.
}


\value{
none
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{OUTPUTS}}, \code{\link{OFCOUNTER}}
}


\examples{
library(wfr)
library(ggplot2)

print(OFCOUNTER)
print(OUTPUTS)

df1=data.frame(A=c("a","a","b3"),
               B=c("b1","b2","b3"),
               C1=1:3,C2=2:4)
\donttest{
  tmpDir = tempdir()
  ofn = file.path(tmpDir, "tab.1.csv")
  saveOutput(df1, oFileName=ofn, caption="this is a wrong caption")
  print(OFCOUNTER)
  print(OUTPUTS)
  ofcmm()
  print(OFCOUNTER)
  saveOutput(df1, oFileName=ofn, caption="this is the correct caption")
  print(OUTPUTS)
}

print(OFCOUNTER)
print(OUTPUTS)

\donttest{
  ofn = file.path(tmpDir, "fig.1.png")
  saveOutput(qplot(1:10,1:10), oFileName=ofn, caption="this is a testing plot")
}

print(OFCOUNTER)
print(OUTPUTS)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }% use one of  RShowDoc("KEYWORDS")
