\name{saveOutput}
\alias{saveOutput} \alias{OBJ2} \alias{OFCOUNTER} \alias{OUTPUTS} \alias{Global.Variable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Save the current R image or the object rds file and add its information to the matrix OUTPUTS
}
\description{
Save the current R image or the object rds file and optionally other formats of the object (e.g. csv for tables and png for figures), its caption, and other inforamtion related to its appearance in the R markdown file, to the matrix OUTPUTS
}
\usage{
saveOutput(obj,ofName=NA, saveWorkspace=F, oPath='.', caption=NA ,
           rmdInd=NA, eval=T,objID=NA, header=NA, footer=NA,
           rowHeaderInd=NA, colWidths=NA, fontSize=11, nRowScroll = 20,
           nRowDisplay = 200, maxTableWidth = 7.2, theme = "zebra",...)
}


\arguments{
   \item{obj}{
    The target object, usually a data.frame or ggplot object. After passing in, its value assigns to the global variable \code{\link{OBJ2}}; therefore, later when the R image is loaded, this target object can be just referred as \code{OBJ2}.
  }

  \item{ofName}{
    (character, NA). The file name of the text (e.g. csv) or image (e.g. png) file of the \code{obj} to save.
  }

  \item{saveWorkspace}{
    (boolean, F)
    \itemize{
        \item F: Save the rds file of the \code{obj} only. If \code{ofName} is provided, the rds file name is \code{paste0(ofNamePrefix,'.rds')}; otherwise,

        \code{paste0(sprintf} \code{("\%03d", OFCOUNTER),'.rds')}.
        \item T: Save the R image of the workspace where \code{obj} is generated as \code{paste0(sprintf} \code{("\%03d", OFCOUNTER),'.r.image.rdata')}
    }
  }


  \item{oPath}{
    (character, '.'). The output path.
  }

  \item{caption}{
    (character, NA). The caption of the target object.
  }

  \item{rmdInd}{
    (integer, \code{\link{OFCOUNTER}}). The order to display \code{obj} in the Rmd file.
  }

  \item{eval}{
    (boolean, T). Should the \code{obj} be included/evaluated in the Rmd file?
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

  \item{...}{
    parameters passed to \code{\link[utils]{write.csv}} to save the \code{obj}, if a data.frame or matrix, as text file.
  }
}


\details{
For the simpilicity of coding, three global variables are created for this function:
\itemize{
    \item{OFCOUNTER}{
    (integer,1). A global variable to count the number of outputs, initial value is 1.
  }

  \item{OBJ2}{
    (any data type, NULL). A global variable to hold the value of \code{obj} inside \code{saveOutput}
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
        \item If \code{ofName} is provided, save \code{obj} as \code{paste0(sprintf("\%03d", OFCOUNTER),'.',ofName)} in the \code{oPath} directory, using either \code{utils::write.csv} or \code{ggplot2::ggsave} depending on the data type of \code{obj}
        \item Create the matrix \code{OUTPUTS} if it is NULL, and assign the values of all other parameters to corresponding columns in \code{OUTPUTS}
        \item Assign \code{obj} to the global variable \code{\link{OBJ2}}, so that when \code{obj} is passed in as a subset of a data.frame, e.g. \code{df1[1:5,]}, that subset can be easily referred using \code{\link{OBJ2}}.
        \item Increment \code{\link{OFCOUNTER}} by 1
    }
}


\value{
none
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{OUTPUTS}}, \code{\link{OFCOUNTER}}, \code{\link{OBJ2}}
}


\examples{
library(wfr)
library(ggplot2)

print(OFCOUNTER)
print(OUTPUTS)
print(OBJ2)

df1=data.frame(A=c("a","a","b3"),
               B=c("b1","b2","b3"),
               C1=1:3,C2=2:4)
saveOutput(df1, ofName="tab.1.csv", caption="this is a testing table")

print(OFCOUNTER)
print(OUTPUTS)
print(OBJ2)

saveOutput(qplot(1:10,1:10), ofName="fig.1.png", caption="this is a testing plot")

print(OFCOUNTER)
print(OUTPUTS)
print(OBJ2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table }% use one of  RShowDoc("KEYWORDS")
