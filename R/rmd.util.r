
# functions used in saving R obj and creating a file containing info of all R objs ------------
#overwrite OUTPUTS.fn in your R script of creating and saving R objs.

OUTPUTS=data.frame()
OFCOUNTER=1

saveWfrInfo=function(rdsFileName){
    pEnv <- parent.frame()
    saveRDS(list(ofc=get('OFCOUNTER',pEnv),outp=pEnv$OUTPUTS),file = rdsFileName)
}

restoreWfrInfo=function(rdsFileName){
    info1=readRDS(rdsFileName)
    pEnv <- parent.frame()
    assign('OFCOUNTER',info1$ofc,pEnv)
    assign('OUTPUTS',info1$outp,pEnv)
}

saveOutput=function(obj=NULL,oFileName,saveWorkspace=FALSE,oPath=getwd(),caption=NA,rmdInd=NA, eval=TRUE,objID=NA,header=NA,
                    footer=NA,rowHeaderInd=NA,colWidths=NA,fontSize=11,nRowScroll = 20,
                    nRowDisplay = 200, maxTableWidth = 7.2, theme = "zebra", numberOutputFiles=TRUE,...){

    pEnv <- parent.frame()
    #pEnv=globalenv()
    counter2 = get('OFCOUNTER', pEnv)
    counterStr = NULL
    if (!is.null(obj) && numberOutputFiles) {
        counterStr = paste0(sprintf("%03d", counter2), '.')
    }

    if (is.na(rmdInd)) {
        rmdInd = counter2
    }

    dirName = dirname(oFileName)
    oBaseName = paste0(counterStr, basename(oFileName))

    if (dirName != '.') {
        #contains path
        if (substr(dirName, 1, 2) == paste0('.', .Platform$file.sep)) {
            # rative path ./pp/kk.txt
            oPath = file.path(getwd(), substr(dirName, 3, nchar(dirName)))
        } else{
            # absolute path
            oPath = dirName
        }
    }

    rdsFn = NA
    objPref = 'unknown'
    if (!is.null(obj)) {
        ofNameToken = unlist(strsplit(oBaseName, '.', TRUE))
        ofNamePrefix = paste(ofNameToken[-length(ofNameToken)], collapse = '.')
        rdsFn = paste0(ofNamePrefix, '.rds')
        saveRDS(obj, file.path(oPath, rdsFn))

        rdsOnly = endsWith(tolower(oFileName), 'rds')

        if (is.data.frame(obj) || is.matrix(obj)) {
            if(!rdsOnly){
                write.csv(obj, file = file.path(oPath, oBaseName), ...)
            }
            objPref = 'tab'
        } else if (is.ggplot(obj)) {
            if(!rdsOnly){
                ggsave(file.path(oPath, oBaseName), obj, dpi = 600, ...)
            }
            objPref = 'fig'
            nRowScroll = nRowDisplay = maxTableWidth = theme = NA
        } else {
            warning("the obj is not a table or ggplot!, saving RDS file only")
        }
    }

    if (saveWorkspace) {
        rImageFileName = paste0(oBaseName, 'r.image.rdata')
        save.image(file.path(oPath, rImageFileName))
    }


    if (is.na(objID)) {
        objID = paste0(objPref, counter2)
    } else{
        objID = gsub("[^A-Za-z0-9]", "", objID)
        objID = paste0(objPref, objID)
    }


    if (counter2 > 1 &&
        objID %in% pEnv$OUTPUTS[1:(counter2 - 1), "objID"]) {
        warning("duplicate objID, OFCOUNTER is appended.")
        objID = paste0(objID, counter2)
    }

    row1 = list(
        oPath = oPath,
        rdsFileName = rdsFn,
        oFileName = oBaseName,
        caption = caption,
        rmdInd = rmdInd,
        eval = eval,
        objID = objID,
        header = header,
        footer = footer,
        rowHeaderInd = rowHeaderInd,
        colWidths = colWidths,
        fontSize = fontSize,
        nRowScroll = nRowScroll,
        nRowDisplay = nRowDisplay,
        maxTableWidth = maxTableWidth,
        theme = theme
    )

    if(counter2>1){
        pEnv$OUTPUTS = rbind.data.frame(pEnv$OUTPUTS[1:(counter2 - 1), ], row1, stringsAsFactors = FALSE)
    }else{
        pEnv$OUTPUTS=data.frame()
        pEnv$OUTPUTS = rbind.data.frame(pEnv$OUTPUTS, row1, stringsAsFactors = FALSE)
    }


    #OFCOUNTER <- OFCOUNTER + 1
    assign('OFCOUNTER', counter2 + 1, pEnv)

    cat('------------------------------finish',
        ifelse(is.na(rdsFn), oFileName, rdsFn),
        "\n")
}


writeExcel=function(fileName,...){
    #library(openxlsx)
    pEnv <- parent.frame()
    df1=get('OUTPUTS',pEnv)
    urls=file.path(df1[,"oPath"],df1[,"oFileName"])
    names(df1[,"oFileName"])=urls
    class(df1[,"oFileName"])="hyperlink"
    write.xlsx(df1,fileName,...)
}


# functions to create Rmd file -------------
createRmd=function(outputRmdFn,outputListFn,
                   rmdTemplateFn=system.file("extdata", "rmd.template.Rmd", package = "wfr"),
                   libs=c("ggplot2"),tabPars=NULL){
    if(!is.null(tabPars)) {
        tabPars = paste0(',', tabPars)
    }
    tabPars=paste0(",isDocx",tabPars)
    figCap=', fig.cap=opts_current$get("fig.cap")'

    #library(openxlsx)
    file.copy(rmdTemplateFn,outputRmdFn,overwrite = TRUE)
    oFile=file(outputRmdFn,'at')
    oLine=paste0('\n\n```{r include=FALSE}
    setwd("',getwd(),'")
    loadLibs(c("',paste0(libs,collapse='","'),'"))
    df1=read.xlsx("',outputListFn,'")\n```')
    writeLines(oLine,oFile)

    outputDf=read.xlsx(outputListFn)
    outputDf=outputDf[which(outputDf[,"eval"]),]
    outputDf=outputDf[order(outputDf$rmdInd),]
    for(i in 1:nrow(outputDf)){
        if(startsWith(outputDf[i,1],'#')){
            writeLines(paste0("\n",outputDf[i,1],"\n"),oFile)
        }else{
            objID=outputDf[i,"objID"]
            if(grepl("^tab",outputDf[i,"objID"])){
                oLine=paste0('\n`r tRef("',objID,'",isDocx)` is the table of ', outputDf[i,"caption"],"\n")
                oLine=paste0(oLine,'\n```{r ',objID,"}\n    showObj(df1,\"",objID,"\"",tabPars,")\n```")
            }else{
                oLine=paste0('\nFigure \\@ref(fig:',objID,') is the figure of ', outputDf[i,"caption"],"\n")
                oLine=paste0(oLine,'\n```{r ',objID,figCap,"}\n    showObj(df1,\"",objID,"\")\n```")
            }

            writeLines(oLine,oFile)
        }

    }

    close(oFile)
}

# utility function used in Rmd file -------------------
nDecimal=function(x){
    h=strsplit(as.character(x), ".",fixed = TRUE)
    sapply(h, function(x){
        if(is.na(x[1])){
            NA
        }else if(length(x)==1){
            0
        }else{nchar(x[2])}
    })
}

# digits only goes with scientific notation
setNsmall=function(v,nSmall){
    if(max(abs(v),na.rm = TRUE)>999){
        format(round(v,digits = nSmall),big.mark = ',',nsmall = nSmall)
    }else{
        format(round(v,digits = nSmall),nsmall = nSmall)
    }
}


isVecNumeric=function(v){
    isNum=is.numeric(v)
    if(!isNum){
        if(is.character(v) || is.factor(v)){
            v2=as.character(v)
            v2=v2[!sapply(v2,is.empty)]
            isNum=all(!is.na(suppressWarnings(as.numeric(v2))))
        }else{
            isNum=FALSE
        }
    }
    isNum
}


num2formattedStr=function(v){
    if(is.character(v) || is.factor(v)){
        v2=as.character(v)
        if(isVecNumeric(v2)){
            v=as.numeric(v2)
        }
    }

    if(is.numeric(v)){
        v2=abs(v)
        vMin = min(v2,na.rm = TRUE)
        vMed = median(v2,na.rm = TRUE)
        vMax = max(v2,na.rm = TRUE)

        if(all(is.na(v))){
            v
        }else if(vMed>=1000000 || vMed < 0.01){
            format(v,scientific = TRUE,digits = 3)
        }else if(vMed>=100 || all(nDecimal(v) %in% c(0,NA))){
            # 2nd case is integers in (-999,999) but has decimal
            #   points in representation, i.e. 30.00
            setNsmall(v,0)
        }else if(vMed>=10){
            setNsmall(v,1)
        }else if(vMed>=1){
            setNsmall(v,2)
        }else if(vMin>=0.1){
            setNsmall(v,2+(max(v,na.rm = TRUE)<=1))
        }else if(vMin>=0.01){
            setNsmall(v,3)
        }else{
            format(v,scientific = TRUE,digits = 3)
        }
    }else{ v }
}



str2list=function(x){
    kk=unlist(strsplit(x,'||',fixed = TRUE))
    h=sapply(kk, function(x){strsplit(x,'|',fixed = TRUE)})
    lapply(h,sapply,trimws)
}


is.empty=function(x){
    #is.na(matrix()) is TRUE
    if(any(is.list(x),is.factor(x))) return(all(length(x)==0))
    if(is.character(x)) return(all(trimws(x) %in% c("","NaN","Inf","-Inf","NA",".")))

    return(all(is.null(x) | is.na(x) | is.infinite(x) | is.nan(x)))
}


loadLibs=function(failedPackages){
    #require() won't try other paths if there is loading error in the first path;
    #It tries different paths only if the package is absent from the current path.
    for(i in .libPaths()){
        if(length(failedPackages)>0){
            loadDone=sapply(failedPackages, require, character.only=TRUE, quietly=TRUE, lib.loc=i)
            failedPackages=names(loadDone)[!loadDone]
        }
    }

    if(length(failedPackages)>0){
        stop(failedPackages, " are not installed!")
    }
}



.tableCaption <- captioner::captioner(prefix = "Table")

tCap = function(cap,label,isDocx) {
    if(isDocx){
        .tableCaption(label, cap)
        #.tableCaption("mylabel", "mycap")
    }else{
        cap
    }
}

tRef <- function(label,isDocx) {
    if(isDocx) {
        stringr::str_extract(.tableCaption(label), "[^:]*")
    }else{
        paste0("Table \\@ref(tab:",label,")")
    }
}

rmdTable=function(dataDf,header=list(colnames(dataDf)), footer=NULL,colWidths=NULL,fontSize=11,
                  caption=NULL,rowHeaderInd=NULL,isDocx=TRUE,nRowScroll=20, nRowDisplay=200,
                  maxTableWidth=7,theme=c("zebra","box","booktabs","vanilla","tron","vader"),
                  char2space=NULL, splitCamelCase=FALSE, footerFontSize=9,minFontSize=9){
    # dataDf: the data frame
    # header: can be a string or a list of one or more character vectors
    #   string: use '|' to separate cell, and '||' to separate rows.
    #   a list: each vector is the column title. The last vector replaces the colnames(dataDf)
    #   neighboring cells of identical content in the header will be merged into one cell.
    #     for myKable(), merge is only horizontal, and on all rows but the last one.
    # footer: can be a string or a list of one or more character vectors
    #   string: use '|' to separate cell, and '||' to separate rows.
    #   a list: each vector contains the following items:
    #   1. optional, The cell content which the footnote refers to. if #4 is 'body', only the first two columns
    #        are searched against #1
    #   2. a character string, the content of the footer. use $~i$ and $^i$ to represent the sub/super-script of i
    #   3. optional, a character, the super-script of the header cell at c(x,y)
    #   4. optional, "header" or "body"
    # colWidths: for myFlexTable only, the column widths, a numerical vector of the length of ncol(dataDf).
    #   set only when footer is added; or a string like "2,1,1,1"
    # caption: the caption of the table. For html output: Table x.x and cross-reference URL is automatically added
    #   to the begining of caption. For docx output: there is no cross-reference URL.
    # rowHeaderInd: for the table body, the indices of columns where neighboring cells of identical content in each
    #   column may be collapsed.
    # isDocx: is the output format docx; if FALSE, html.
    # nRowScroll: for html output only, the cutoff on nuber of rows to apply a scroll window.
    # nRowDisplay: the max number of rows to print.

    # Example:
    # df1=data.frame(matrix(1:9,nrow = 3))
    # df1=cbind(VID=c("top2","top2","3rd"),df1)
    # header=list(c("VID","VID","color code","color code"),c("VID","VID","code1","code2"))
    # footer=list(
    #     list("VID","vechile ID","a","header"),
    #     list("color code","vechile color code$^ref$","b","header"),
    #     list("code1","color code 1$~ref$","c","header"),
    #     list("code2","color code 2$~ref$","d","header"),
    #     list("top2","color code 2$~ref$","e","body")
    # )
    ## footer=c("$^a$VID: vechile ID || $^b$color code: vechile color code$~ref$")
    ## footer=c("VID: vechile ID || color code|vechile color code$^ref$|b|header || top2|color code 2$~ref$|e|body")
    # colWidths=c(1,2,1,1)
    # rmdTable(df1,header,footer,colWidths,caption = "my cap",isDocx = FALSE)
    if(isDocx && nrow(dataDf)>nRowDisplay){
        dataDf=dataDf[1:nRowDisplay,]
        caption=paste0(caption," (top ", nRowDisplay, " rows only)")
    }

    if(is.matrix(dataDf)) {
        dataDf=as.data.frame(dataDf)
    }

    for(i in 1:ncol(dataDf)){
        #if(is.integer(dataDf[,i]) | is.numeric(dataDf[,i])){
            #if dataDf is a numeric matrix, the 1st run of this line changes dataDf to a char matrix,
            #  i.e. following run of this line won't have any effect.
            dataDf[,i]=num2formattedStr(dataDf[,i])
        #}
    }

    if(is.character(colWidths)){
        colWidths=as.numeric(unlist(strsplit(colWidths,',',fixed = TRUE)))
    }

    # if(is.null(header)){
    #     # colName=colnames(dataDf)
    #     # colName=gsub("[_\\.]"," ",colName)
    #     # # separate camel case
    #     # colName=gsub("(?<=[a-z])(?=[A-Z])", " \\1", colName, perl = TRUE)
    #     # header=list(colName)
    #     header=list(colnames(dataDf))
    # }
    #
    if(isDocx){
        myFlexTable(dataDf,header=header, footer=footer,colWidths=colWidths,fontSize=fontSize,
                    caption=caption,rowHeaderInd=rowHeaderInd,maxTableWidth=maxTableWidth,
                    theme = theme,
                    char2space=char2space, splitCamelCase=splitCamelCase,
                    footerFontSize=footerFontSize,minFontSize=minFontSize)
    }else{
        myKable(dataDf,header=header, footer=footer,caption=caption,
                rowHeaderInd=rowHeaderInd,nRowScroll=nRowScroll,theme = theme)
    }

}

#h1: a char vector
# v=c('a','b','b','c','d','d')
.getEndIndOfDifferentCell=function(v){
    borderInd=NULL
    if(length(v)>1){
       for (i in 1:(length(v)-1)) {
            if(v[i]!=v[i+1]){
                borderInd=c(borderInd,i)
            }
        }
    }

    borderInd
}

# break a string at a non-letter position into two substrings, so that the longest substring
#  after breaking is the shortest among all possible breaking points.
breakRatio=function(aStr){
    inds=gregexpr("[^a-zA-Z]",aStr)[[1]]
    nc1=nchar(aStr)
    d1=abs(inds-nc1/2)
    i=(which(d1==min(d1)))[1]
    maxSubLen=max(inds[i],nc1-inds[i])
    maxSubLen/nc1
}


.nAveUniqueContinuiousValue=function(v){
    # get the average number of cells whose have identical content and are next to each other
    nUni=1
    if(length(v)>1){
        if(!is.character(v)){
            v=as.character(v)
        }

        for(i in 2:length(v)){
            if(v[i]!=v[i-1]){
                nUni=nUni+1
            }
        }
    }

    floor(length(v)/nUni)
}

setWidths=function(x, header1, maxTableWidth=7,rowHeaderInd=NULL,minFontSize=9,
                   nRowPerRowHeader=NULL){
    stopifnot(inherits(x, "flextable"))
    newFontSize=x$body$styles$text$font.size$data[1,1]
    #cell padding in inch
    CP=0.15

    # the widths here isn't for the header1 only, but for all rows in the header
    #   but only widths of header1 is needed here since usually other rows are
    #   of multicell-rows.
    #wHeader=dim_pretty(x,"header")$widths
    # this is the widths of only header1, but not accounting for theme, which differ from
    #   hw0, i.e. those of accounting for theme, by ~1.3 times. Therefore, wHeader need
    #   to use hw0 except those cells where the cells on top are multicells.
    wHeader=(delete_part(x) %>% add_header_row(values = header1) %>% dim_pretty(part="header"))$widths*1.3
    hw0=dim_pretty(x,"header")$widths
    sInds=which(hw0/wHeader<1.2)
    wHeader[sInds]=hw0[sInds]
    #add 0.05 inch so columns won't be too tight
    wBody=dim_pretty(x,"body")$widths + CP
    if(!is.null(rowHeaderInd)){
        #increase 10% for being bold font
        wBody[1:rowHeaderInd]=wBody[1:rowHeaderInd]*1.1
    }

    letterOnlyInds=grep("^[a-z]*$",header1,ignore.case = TRUE)

    wRatios=(wHeader-wBody)/wBody
    indsH=which(wRatios>0)

    HBWidths=mapply(max, wHeader,wBody)
    if(sum(HBWidths)<=maxTableWidth){
        wBody2=HBWidths
    }else if(sum(HBWidths)/maxTableWidth < 1.08){
        # if table can fit by reducing font size by 1, do that instead of
        #   wrapping headers.
        wBody2=HBWidths*0.92
        newFontSize=newFontSize-1
    }else{
        wBody2=wBody
        wHeader2=wHeader
        indsH2=intersect(indsH,letterOnlyInds)
        for(i in indsH2){
            wBody2[i]=max(wBody[i],wHeader[i]+CP)
        }

        indsH2=setdiff(indsH,letterOnlyInds)
        for(i in indsH2){
            #wrap headers
            ratio1=breakRatio(header1[i])
            # the closer ratio1 is to 1, the more likely ratio1*1.3 > 1
            #wHeader2[i]=wHeader[i]*ratio1*1.3
            wHeader2[i]=min(wHeader[i]*ratio1 + CP, wHeader[i]*0.9)
            # 1.3 is to give more room
            wBody2[i]=max(wBody[i],wHeader2[i])
        }

        #wrap rowHeader if they are multi-cells
        if(sum(wBody2)>maxTableWidth){
            for(i in rowHeaderInd){
                d1=sum(wBody2)-maxTableWidth
                c1=nRowPerRowHeader[i]
                if(c1>1){
                    wBody2[i]=wBody2[i]/c1 + max(wBody2[i]/c1-d1,0)
                    wBody2[i]=max(wBody2[i],wHeader2[i])
                }
            }
        }

        # reduce font size until fit instead of wrapping table body or
        #   futher wrapping headers
        if(sum(wBody2)>maxTableWidth){
            fsr=sum(wBody2)/maxTableWidth - 1
            # character length reduces by 8% per font size
            fsr=min(ceiling(fsr/0.08), newFontSize-minFontSize)
            wBody2=wBody2*(1-fsr*0.08)
            wHeader2=wHeader2*(1-fsr*0.08)
            newFontSize=newFontSize-fsr

            # still does not fit at minFontSize, wrap the table body, staring from
            #   the longest table columns.
            bLenInds=order(wBody2,decreasing = TRUE)
            i=1
            while(sum(wBody2)>maxTableWidth && i<=length(bLenInds)){
                j=bLenInds[i]
                if(!(j %in% rowHeaderInd)){
                    d1=sum(wBody2)-maxTableWidth
                    # set wBody2[j] exactly by half may cause wrapping into three lines
                    #   therefore setting a value slightly smaller than 2
                    c1=ifelse(wBody2[j]/2<d1,1.95,2)
                    wBody2[j]=wBody2[j]/c1 + max(wBody2[j]/c1-d1,0)
                    wBody2[j]=max(wBody2[j],wHeader2[j])
                }

                i=i+1
            }

            # while(sum(wBody2)>maxTableWidth && i<=length(bLenInds)){
            #     j=bLenInds[i]
            #     d1=sum(wBody2)-maxTableWidth
            #     #c1=ifelse(d1/wBody2[i]>0.5 && i <= rowHeaderInd,2.95,1.95)
            #     wBody2[j]=wBody2[j]/c1 + max(wBody2[j]/c1-d1,0)
            #     wBody2[j]=max(wBody2[j],wHeader[j])
            #     i=i+1
            # }
        }
    }

    list(widths=wBody2,fs=newFontSize)
}




setFlexTableFontSize=function(ft,fontSize,footerFontSize=9){
    fontsize(ft,size=fontSize,part = "all") %>% fontsize(size=footerFontSize,part = "footer") %>%
        height_all(0.02*(footerFontSize),part = "footer")
}


myFlexTable=function(dataDf,header=list(colnames(dataDf)), footer=NULL,colWidths=NULL,
                     fontSize=11,caption=NULL,rowHeaderInd=NULL,mergeBodyColumn=TRUE,
                     maxTableWidth=7,theme=c("zebra","box","booktabs","vanilla","tron","vader"),
                     char2space=NULL, splitCamelCase=FALSE,footerFontSize=9,minFontSize=9){
    # create a table used in Rmd file that can output correctly to common format, esp. docx, whereas kable() cannot.
    #   uses flextable package: https://davidgohel.github.io/flextable/articles/overview.html
    #   similar packages: https://hughjonesd.github.io/huxtable/huxtable.html

    # library(flextable)
    # library(magrittr)
    # library(officer)

    if(!is.null(char2space) || splitCamelCase){
        header1=header[[length(header)]]

        if(!is.null(char2space)){
            header1=gsub(char2space,' ',header1)
        }

        if(splitCamelCase){
            header1=gsub("(?<=[a-z])(?=[A-Z])", " \\1", header1, perl = TRUE)
        }

        header[[length(header)]]=header1
    }


    rownames(dataDf)=NULL
    if(is.matrix(dataDf)) dataDf=as.data.frame(dataDf)

    #avoid "duplicated col_key" error from flextable in case some column names are identical
    colnames(dataDf)=paste0('C',1:ncol(dataDf))
    ft <- flextable(dataDf)
    if(!is.empty(header)){
        if(is.character(header)){
            header=str2list(header)
        }
        ft=delete_part(ft)
        for (i in header) {
            ft=add_header_row(ft,top=FALSE,values = i)
        }
        ft = merge_v(ft, part = "header")
    }

    ft = merge_h(ft, part = "header")
    if(theme[1]=="box"){
        ft=theme_box(ft)
    }else{
        if(theme[1]=="zebra"){
            ft=theme_zebra(ft,odd_body = "gray88") %>% border_outer(border = fp_border(color="grey"))
        }else{
            themeFun = get(paste0("theme_",theme[1]))
            ft=themeFun(ft)
        }

        if(length(header)>1){
            borderInd=.getEndIndOfDifferentCell(header[[1]])
            if(!is.null(borderInd)){
                #borderInd=borderInd-1
                ft=vline(ft,j=borderInd, border = fp_border(color="grey"))
            }
        }
    }

    rhInds=`if`(is.empty(rowHeaderInd),NULL,1:rowHeaderInd)
    if(!is.empty(rowHeaderInd)){
        ft = merge_v(ft, j=rhInds, part = "body") %>% bold(i=1:nrow(dataDf),j=rhInds)

        if(theme[1]!="box"){
            borderInd=.getEndIndOfDifferentCell(dataDf[,1])
            if(!is.null(borderInd)){
                # borderInd=borderInd-1
                ft=hline(ft,i=borderInd, border = fp_border(color="grey"))
            }
        }

        if(mergeBodyColumn && length(rhInds)>1){
            rowInds=apply(as.matrix(dataDf[,rhInds]), 1,
                          function(v){h=length(v);any(v[1:(h-1)]==v[2:h])})
            if(any(rowInds)){
                ft = merge_h(ft, i=which(rowInds), part = "body")
            }
        }
    }

    if(!is.empty(footer)){
        makeFooterChunk=function(x){
            switch (substr(x,1,1),
                    '^' = as_chunk(substr(x,2,nchar(x)), props = fp_text(vertical.align = "superscript", font.size = fontSize)),
                    '~' = as_chunk(substr(x,2,nchar(x)), props = fp_text(vertical.align = "subscript", font.size = fontSize)),
                    x)
        }

        convertOneFooterStr=function(ftStr){
            ftVec=unlist(strsplit(ftStr,split = "$",fixed = TRUE))
            lapply(ftVec,makeFooterChunk )
        }

        getInd=function(aList,x){
            kk=sapply(aList, function(x,y){match(y,x)},x)
            k=which(!is.na(kk))
            c(k[1],kk[k[1]])
        }

        if(is.character(footer)){
            footer=str2list(footer)
        }

        for(i in footer){
            if(length(i)==1){
                ft=flextable::footnote(ft,value=as_paragraph(list_values=convertOneFooterStr(i)),ref_symbols ="") # ,part = "header",
            }else{
                if(i[4]=="header"){
                    inds=getInd(header,i[1])
                }else{
                    inds=getInd(as.list(as.data.frame(t(dataDf))),i[1])
                }

                # else if(is.empty(rowHeaderInd)){
                #     stop("rowHeaderInd must be defined if footer contains 'body'!")
                # }else{
                #     inds=getInd(as.list(as.data.frame(t(dataDf[,rowHeaderInd]))),i[1])
                # }

                ft=flextable::footnote(ft,inds[1], inds[2], as_paragraph(list_values=convertOneFooterStr(i[2])), i[3], i[4])
            }
        }
    }

    ft=setFlexTableFontSize(ft,fontSize,footerFontSize)

    newFontSize=NULL
    if(is.empty(colWidths)){
        tmpV=NULL
        for(x in rhInds){
            tmpV=c(tmpV,.nAveUniqueContinuiousValue(dataDf[,x]))
        }
        swRe=setWidths(ft,header[[length(header)]],maxTableWidth,rowHeaderInd,minFontSize,tmpV)
        colWidths = swRe$widths
        newFontSize = swRe$fs
    }

    if(!is.null(newFontSize)){
        ft=setFlexTableFontSize(ft,newFontSize,footerFontSize)
    }

    if(!is.empty(caption)){
        if(grepl("Table",caption,ignore.case = TRUE)){
            # increases dim_pretty(ft,"header")$widths[1]. So it MUST be placed after setWidths()
            # seems add_header_lines use default font size 10, regardless of the actual font size
            # of the table.
            ft=add_header_lines(ft,caption)
        }else{
            ft=set_caption(ft, caption)
        }
    }

    if(is.null(newFontSize)){
        # Therefore its font size is changed to be same as the table header.
        ft=fontsize(ft,size=fontSize,part = "header")
    }else if(newFontSize>10){
        ft=fontsize(ft,size=newFontSize,part = "header")
    }

    ft= align_nottext_col(ft,align = "center",footer = FALSE) %>%
        align_text_col(align = "center",footer = FALSE) %>% width(width=colWidths)

    ft
}

# h=list(c('a','b','c','c'),c('a','b','c1','c2'))
# hh=.setKableHeader(h)
.setKableHeader=function(header){
    if(length(header)>1){
        header2=list()

        for (i in 1:(length(header)-1)) {
            uniLens=1
            h1=header[[i]]
            h2=header[[i+1]]
            for(j in 1:(length(h1)-1)){
                if(h1[j]==h1[j+1]){
                    uniLens[length(uniLens)]=uniLens[length(uniLens)]+1
                }else{
                    uniLens=c(uniLens,1)
                }
            }
            names(uniLens)=unique(h1)

            for(j in 1:length(names(uniLens))){
                if(names(uniLens)[j]==h2[j]){
                    names(uniLens)[j]= " "
                }
            }
            header2[[i]]=uniLens
        }

        header2[[i+1]]=header[[i+1]]
        header2
    }else{ header }
}

myKable=function(dataDf,header=list(colnames(dataDf)), footer=NULL,caption=NULL,
                 rowHeaderInd=NULL,nRowScroll=20,theme=c("zebra","box", "vanilla")){
    # for table output in html format in Rmd file
    # library(kableExtra)
    rownames(dataDf)=NULL

    if(is.matrix(dataDf)) dataDf=as.data.frame(dataDf)
    #if(is.empty(header)) header=list(colnames(dataDf))
    if(is.character(header)) header=str2list(header)

    for(i in 1:ncol(dataDf)){
        if(is.factor(dataDf[,i])) dataDf[,i]=as.character(dataDf[,i])
    }

    if(!is.empty(rowHeaderInd)){
        rowHeaderInd=1:rowHeaderInd
    }

    if(!is.empty(footer)){
        if(is.character(footer)){
            footer=str2list(footer)
        }

        for(i in footer){
            if(length(i)==4){
                if(i[4]=="header"){
                    aList=header
                }else{
                    aList=dataDf
                }

                # else if(is.empty(rowHeaderInd)){
                #     stop("rowHeaderInd must be defined if footer contains 'body'!")
                # }else{
                #     aList=dataDf[,rowHeaderInd]
                # }
                #indList: a list of int vectors containing the indices of item equal to i[1]
                #  e.g. if indList[[3]][1]==2, dataDf[3,2]==i[1]
                indList=lapply(aList, function(aVec,x){which(aVec==x)},i[1])
                for(j in 1:length(indList)){
                    if(length(indList[[j]])>0){
                        aList[[j]][indList[[j]]]=paste0(aList[[j]][indList[[j]]],'^',i[3],'^')
                    }
                }

                if(i[4]=="header"){
                    header=aList
                }else{
                    dataDf[,1:2]=do.call(cbind.data.frame, list(aList,stringsAsFactors=FALSE))
                }
            }
        }
    }

    if(length(header)>0){
        header=.setKableHeader(header)
        colnames(dataDf)=header[[length(header)]]
        header[length(header)]=NULL
    }

    kTheme=switch (theme[1],box="bordered",zebra="striped","basic")
    ft <- kable(dataDf,caption = caption, escape = TRUE,align = "c") %>% kable_styling(kTheme)

    if(length(header)>0){
        for (h1 in header) {
            ft=add_header_above(ft,h1)
        }
        ft=row_spec(ft,row=0,align = 'c')
    }

    if(!is.empty(rowHeaderInd)){
        ft=column_spec(ft,rowHeaderInd, bold = TRUE) %>% collapse_rows(ft,columns = rowHeaderInd, valign = "middle")
    }

    if(!is.empty(footer)){
        makeFooterChunk=function(x){
            c1=substr(x,1,1)
            if(c1 %in% c('^','~')) return(paste0(x,c1))
            return(x)
        }

        #a footerStr is i[2] of a footer, a footer is a char vec of length 4
        convertOneFooterStr=function(ftStr){
            ftVec=unlist(strsplit(ftStr,split = "$",fixed = TRUE))
            ftVec=sapply(ftVec,makeFooterChunk )
            paste(ftVec,collapse = "")
        }

        convertOneFooter=function(ftVec){
            if(length(ftVec)==1){
                convertOneFooterStr(ftVec)
            }else{
                ftVec[2]=convertOneFooterStr(ftVec[2])
                paste0('^',ftVec[3],'^',ftVec[1],": ",ftVec[2])
            }
        }

        ft=kableExtra::footnote(ft,general = sapply(footer,convertOneFooter),general_title ="")
    }

    if(nrow(dataDf)>nRowScroll){
        ft %>% scroll_box(width = "100%", height = "400px")
    }else{ft}
}



showObj=function(oDf,objID=NULL,isDocx=FALSE,...){
    if(is.null(objID)){
        objID=opts_current$get("label")
    }
    oInd=which(oDf[,"objID"]==objID)
    if(!is.na(oDf[oInd,"rdsFileName"])){
        rdsFn = file.path(oDf[oInd,"oPath"],oDf[oInd,"rdsFileName"])
        obj1=readRDS(rdsFn)
    }else{
        obj1=file.path(oDf[oInd,"oPath"],oDf[oInd,"oFileName"])
    }


    tabProp=intersect(names(formals(rmdTable)),colnames(oDf))
    propVal=as.list(oDf[oInd,tabProp])
    dotArgs=list(...)
    propVal[names(dotArgs)]=dotArgs
    naInd=which(sapply(propVal,is.na))
    if(length(naInd)>0){
        propVal=propVal[-naInd]
    }

    if(is.data.frame(obj1) | is.matrix(obj1)){
        propVal$caption=tCap(propVal$caption,objID,isDocx)
        propVal$isDocx=isDocx
        propVal$dataDf=obj1
        do.call(rmdTable,propVal)
    }else{
        if(exists("opts_current")){
            opts_current$set(fig.cap=propVal$caption)
        }
        if(is.character(obj1)){
            knitr::include_graphics(obj1)
        }else{
            obj1
        }
    }
}




# Rmd format function ----------
setHtmlHeaderProperty=function(titleFontSize=NULL,titleFontFamily=NULL,titleColor=NULL,
                               authorFontSize=NULL,authorFontFamily=NULL,authorColor=NULL,
                               dateFontSize=NULL,dateFontFamily=NULL,dateColor=NULL){
    hs=c("title","author","date")
    props=c('font-size','font-family','color')
    names(props)=c('FontSize','FontFamily','Color')
    oLine=paste0('<style type="text/css">',"\n")
    for(i in hs){
        hStr=ifelse(i=='title','h1','h4')
        propVals=mget(paste0(i,names(props)))
        names(propVals)=names(props)
        if(!is.null(propVals[['FontSize']])){
            propVals[['FontSize']]=paste0(propVals[['FontSize']],'px')
        }
        oLine=paste0(oLine,hStr,'.',i," {\n")
        for(j in names(props)){
            oLine=paste0(oLine,props[[j]],': ',propVals[[j]],";\n")
        }
        oLine=paste0(oLine,"text-align: center;\n}\n")
    }
    paste0(oLine,'</style>')
}
