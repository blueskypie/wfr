## ----echo=F, message = F, include= F, warning = T, dpi = 300-------------
oFormat="html"
knitr::opts_chunk$set(echo = F,message = F,warning = F,error=T,include = T,cache=F,fig.align="center",
                      fig.width = 10, fig.height=8, dpi = 300, 
                      out.width = ifelse(oFormat=="pdf","480pt","800px"))


#isDocx = (oFormat!="html")
isDocx=T
library(wfr)
sharedLibs <-c("knitr","pander","xtable","magrittr","flextable","officer","openxlsx")
extraLibs = `if`(isDocx,c("captioner","stringr"),c("kableExtra"))
loadLibs(c(sharedLibs,extraLibs))

## ----tab1,message=T------------------------------------------------------
df1=read.csv(system.file("extdata", "example.table.1.csv", package = "wfr"))
header = list(c('variable','stats','subject','subject','subject'),colnames(df1))
footer = list(c("Note:"),c("subject","Caucasian$^ref_1$ only","a","header"),
              c("Age (years)","One control has no record","b","body"))
rmdTable(df1,header = header, footer=footer,rowHeaderInd = 2,isDocx = isDocx,caption = tCap("Age and Sex statistics","tab1",isDocx))

## ----figWF, fig.cap="Proposed workflow from R script to Rmd file"--------
#knitr::include_graphics(system.file("extdata", "workflow.png", package = "wfr"))
knitr::include_graphics("../inst/extdata/workflow.png")

## ----figSO, fig.cap="Under the hood of `showObj`"------------------------
#knitr::include_graphics(system.file("extdata", "showObj.PNG", package = "wfr"))
knitr::include_graphics("../inst/extdata/showObj.PNG")

## ----figRmdT1, fig.cap="From the original to the final table"------------
#knitr::include_graphics(system.file("extdata", "rmdTable.1.PNG", package = "wfr"))
knitr::include_graphics("../inst/extdata/rmdTable.1.PNG")

## ----figTableTheme, fig.cap="Table themes for Word output"---------------
#knitr::include_graphics(system.file("extdata", "table.themes.png", package = "wfr"))
knitr::include_graphics("../inst/extdata/table.themes.png")

## ----figTableKable, fig.cap="`kable` cannot merge horizontal cells in the 1st row of header and table body"----
#knitr::include_graphics(system.file("extdata", "table.kable.PNG", package = "wfr"))
knitr::include_graphics("../inst/extdata/table.kable.PNG")

## ----tabNoF, echo=T------------------------------------------------------
df1 = read.csv("../inst/extdata/example.format.csv",check.names = F)

flextable(df1) %>% set_caption(caption=tCap("Auto-formatting on numbers using flextable","tabNoF",isDocx)) %>% autofit()

## ----tabF, echo=T--------------------------------------------------------
rmdTable(df1, caption = tCap("Auto-formatting on numbers using num2formattedStr","tabF",isDocx))

## ----tabSW1, echo=F------------------------------------------------------
df1 = read.csv("../inst/extdata/example.table.2.csv",check.names = F)
colnames(df1)=gsub('?','Δ',colnames(df1),fixed = T)
header=list(c("pathway","gene",rep("batch A",3),rep("batch B",3),rep("A vs B",3)),colnames(df1))
footer=list(c("ΔCt Mean","ΔCt is calulated using median value as reference gene for each visit and patient,i.e., ΔCt = Ct - Ct$~median$","a","header"),
            c("Gene 1", "Our target gene$^ref$","b","body"))
rmdTable(df1, caption = tCap("maxTableWidth is set as 7.0 inch (default)","tabSW1",isDocx),
         footer = footer,header = header,rowHeaderInd = 1)

## ----tabSW2, echo=F------------------------------------------------------
rmdTable(df1, caption = tCap("maxTableWidth is set as 6.5 inch ","tabSW2",isDocx),
         maxTableWidth = 6.5,footer = footer,header = header,rowHeaderInd = 1)

## ----tabSW4, echo=F------------------------------------------------------
df1 = read.csv("../inst/extdata/example.table.3.csv",check.names = F)
colnames(df1)=gsub('?','Δ',colnames(df1),fixed = T)
rmdTable(df1, caption = tCap("maxTableWidth is set as 6.5 inch ","tabSW4",isDocx),
         maxTableWidth = 6.5,footer = footer,header = header,rowHeaderInd = 1,minFontSize = 9)

## ----tabSW6, echo=F------------------------------------------------------
df2=df1
df2[,2]=NULL
footer2=footer[1]
header2=list(c("pathway",rep("batch A",3),rep("batch B",3),rep("A vs B",3)),colnames(df2))
rmdTable(df2, caption = tCap("maxTableWidth is set as 6.5 inch ","tabSW6",isDocx),
         maxTableWidth = 6.5,footer = footer2,header = header2,rowHeaderInd = 1,minFontSize = 9)

## ----tabSW7, echo=F------------------------------------------------------
df2=df1
df2[,1]=NULL
footer2=footer
header2=list(c("gene",rep("batch A",3),rep("batch B",3),rep("A vs B",3)),colnames(df2))
rmdTable(df2, caption = tCap("maxTableWidth is set as 6.5 inch and minFontSize as 9","tabSW6",isDocx),
         maxTableWidth = 6.5,footer = footer2,header = header2,minFontSize = 9)

