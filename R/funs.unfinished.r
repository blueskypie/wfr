# docRmd=function(outputRmdFn,sortByDate=FALSE;
#                    rmdTemplateFn=system.file("extdata", "rmd.template.Rmd", package = "wfr"),...){
#     fileNames=list.files(...)
#     if(sortByDate){
#         fInfo = file.info(fileNames)
#         fInfo = fInfo[with(fInfo, order(as.POSIXct(mtime))), ]
#         fileNames = rownames(fInfo)
#     }
#
#     file.copy(rmdTemplateFn,outputRmdFn,overwrite = TRUE)
#     oFile=file(outputRmdFn,'at')
#
#     for(i in fileNames){
#
#     }
#
#     #library(openxlsx)
#     file.copy(rmdTemplateFn,outputRmdFn,overwrite = TRUE)
#     oFile=file(outputRmdFn,'at')
#     oLine=paste0('\n\n```{r include=FALSE}
#     setwd("',getwd(),'")
#     df1=read.xlsx("',outputListFn,'")\n```')
#     writeLines(oLine,oFile)
#
#     outputDf=read.xlsx(outputListFn)
#     outputDf=outputDf[which(outputDf[,"eval"]),]
#     outputDf=outputDf[order(outputDf$rmdInd),]
#     for(i in 1:nrow(outputDf)){
#         if(startsWith(outputDf[i,1],'#')){
#             writeLines(paste0("\n",outputDf[i,1],"\n"),oFile)
#         }else{
#             objID=outputDf[i,"objID"]
#             if(grepl("^tab",outputDf[i,"objID"])){
#                 oLine=paste0('\n`r tRef("',objID,'",isDocx)` is the table of ', outputDf[i,"caption"],"\n")
#                 oLine=paste0(oLine,'\n```{r ',objID,"}\n    showObj(df1,\"",objID,"\"",tabPars,")\n```")
#             }else{
#                 oLine=paste0('\nFigure \\@ref(fig:',objID,') is the figure of ', outputDf[i,"caption"],"\n")
#                 oLine=paste0(oLine,'\n```{r ',objID,figCap,"}\n    showObj(df1,\"",objID,"\")\n```")
#             }
#
#             writeLines(oLine,oFile)
#         }
#
#     }
#
#     close(oFile)
# }
#
#
#
# saveOutput=function(obj=NULL,oFileName=NA,saveWorkspace=FALSE,oPath=getwd(),caption=NA,rmdInd=NA, eval=TRUE,objID=NA,header=NA,
#                     footer=NA,rowHeaderInd=NA,colWidths=NA,fontSize=11,nRowScroll = 20,
#                     nRowDisplay = 200, maxTableWidth = 7.2, theme = "zebra", numberOutputFiles=TRUE,...){
#     if(is.null(obj) && is.na(oFileName)){
#         stop("both obj and oFileName are empty!")
#     }
#
#     if(!numberOutputFiles && is.na(oFileName)){
#         stop("If oFileName are empty, numberOutputFiles must be TRUE!")
#     }
#
#     rdsOnly=
#
#         pEnv <- parent.frame()
#     #pEnv=globalenv()
#     counter2=get('OFCOUNTER',pEnv)
#     counterStr=NULL
#     if(!is.null(obj) && numberOutputFiles){
#         counterStr=paste0(sprintf("%03d", counter2),'.')
#     }
#
#     if(is.na(rmdInd)){
#         rmdInd=counter2
#     }
#
#     if(!is.na(oFileName)){
#         dirName=dirname(oFileName)
#         oBaseName=paste0(counterStr,basename(oFileName))
#
#         if(dirName!='.'){ #contains path
#             if(substr(dirName,1,2)==paste0('.',.Platform$file.sep)){ # rative path ./pp/kk.txt
#                 oPath=file.path(getwd(),substr(dirName,3,nchar(dirName)))
#             }else{ # absolute path
#                 oPath=dirName
#             }
#         }
#     }
#
#     objPref=NULL
#     if(!is.na(oFileName)){
#         tokens1=unlist(strsplit(oFileName,'.',TRUE))
#         objPref=tokens1[length(tokens1)]
#     }
#
#     if(is.data.frame(obj) || is.matrix(obj)){
#         if(!is.na(oFileName)){
#             write.csv(obj, file=file.path(oPath,oBaseName),...)
#         }
#
#         objPref='tab'
#     }else if(is.ggplot(obj)){
#         if(!is.na(oFileName)){
#             ggsave(file.path(oPath,oBaseName),obj,dpi=600)
#         }
#
#         objPref='fig'
#         nRowScroll=nRowDisplay=maxTableWidth=theme=NA
#     }else if(!is.null(obj)){
#         warning("the obj is not a table or ggplot!, saving r.image only")
#     }
#
#     if(!is.null(objPref)){
#         if(is.na(objID)){
#             objID=paste0(objPref,counter2)
#         }else{
#             objID=gsub("[^A-Za-z0-9]","",objID)
#             objID=paste0(objPref,objID)
#         }
#     }
#
#     if(saveWorkspace){
#         if(!is.na(oFileName)){
#             rImageFileName=paste0(oBaseName,'r.image.rdata')
#         }else{
#             rImageFileName=paste0(counterStr,'r.image.rdata')
#         }
#         save.image(file.path(oPath,rImageFileName))
#     }
#
#     rdsFn=NA
#     if(!is.null(obj)){
#         if (!is.na(oFileName)) {
#             ofNameToken = unlist(strsplit(oBaseName, '.', TRUE))
#             ofNamePrefix = paste(ofNameToken[-length(ofNameToken)], collapse = '.')
#             rdsFn = paste0(ofNamePrefix, '.rds')
#         } else{
#             rdsFn = paste0(counterStr, 'rds')
#         }
#         saveRDS(obj, file.path(oPath, rdsFn))
#     }
#
#     if(nrow(pEnv$OUTPUTS)>0 && objID %in% pEnv$OUTPUTS[1:(counter2-1),"objID"]){
#         warning("duplicate objID, OFCOUNTER is appended.")
#         objID=paste0(objID,counter2)
#     }
#
#     row1=list(oPath=oPath, rdsFileName=rdsFn, oFileName=oBaseName, caption=caption,
#               rmdInd=rmdInd, eval=eval, objID=objID, header=header, footer=footer,
#               rowHeaderInd=rowHeaderInd, colWidths=colWidths, fontSize=fontSize, nRowScroll = nRowScroll,
#               nRowDisplay = nRowDisplay, maxTableWidth = maxTableWidth, theme = theme)
#     pEnv$OUTPUTS=rbind.data.frame(pEnv$OUTPUTS[1:(counter2-1),],row1,stringsAsFactors = FALSE)
#
#     #OFCOUNTER <- OFCOUNTER + 1
#     assign('OFCOUNTER',counter2 + 1,pEnv)
#
#     cat('------------------------------finish', ifelse(is.na(rdsFn),oFileName,rdsFn),"\n")
# }
#
# # break a string at a non-letter position into k substrings, so that the longest substring
# #  after breaking is the shortest among all possible breaking points.
# # not done yet
# # .breakRatio1=function(aStr, k=2){
# #     inds=gregexpr("[^a-zA-Z]",aStr)[[1]]
# #     k=min(k,length(inds)+1)
# #     if(k==1){
# #         return(1)
# #     }
# #
# #     nc1=nchar(aStr)
# #     minInd=ifelse(k>2,nc1/k,1)
# #     maxInd=ifelse(k>2,nc1*(k-1)/k,nc1)
# #     inds=inds[which(inds>=minInd && inds<=maxInd)]
# #
# #     d1=abs(inds-nc1/k)
# #     if(k>2){
# #         d1=c(d1,abs(inds-nc1*(k-1)/k))
# #     }
# #
# #     i=(which(d1==min(d1)))[1]
# #     maxSubLen=ifelse(k>2, min(inds[i],nc1-inds[i]),max(inds[i],nc1-inds[i]))
# #     maxSubLen/nc1
# # }
#
#
# # this wraps the header into three lines
# # .setWidths1=function(x, header1, maxTableWidth=7,rowHeaderInd=NULL,minFontSize=9,
# #                      nRowPerRowHeader=NULL){
# #     stopifnot(inherits(x, "flextable"))
# #     newFontSize=x$body$styles$text$font.size$data[1,1]
# #     #cell padding in inch
# #     CP=0.15
# #
# #     # the widths here isn't for the header1 only, but for all rows in the header.
# #     #   However, only widths of header1 is needed here since usually other rows are
# #     #   of multicell-rows.
# #     # wHeader=dim_pretty(x,"header")$widths
# #     #   this is the widths of header1 only, but not accounting for theme, which differ from
# #     #   hw0, i.e. those of accounting for theme, by ~1.3 times. Therefore, wHeader need
# #     #   to use hw0 except those cells where the cells on top are multicells.
# #     wHeader=(delete_part(x) %>% add_header_row(values = header1) %>% dim_pretty(part="header"))$widths*1.3
# #     hw0=dim_pretty(x,"header")$widths
# #     sInds=which(hw0/wHeader<1.2)
# #     wHeader[sInds]=hw0[sInds]
# #     #add 0.05 inch so columns won't be too tight
# #     wBody=dim_pretty(x,"body")$widths + CP
# #     if(!is.null(rowHeaderInd)){
# #         #increase 10% for being bold font
# #         wBody[1:rowHeaderInd]=wBody[1:rowHeaderInd]*1.1
# #     }
# #
# #     letterOnlyInds=grep("^[a-z]*$",header1,ignore.case = TRUE)
# #
# #     wRatios=(wHeader-wBody)/wBody
# #     indsH=which(wRatios>0)
# #
# #     HBWidths=mapply(max, wHeader,wBody)
# #     if(sum(HBWidths)<=maxTableWidth){
# #         wBody2=HBWidths
# #     }else if(sum(HBWidths)/maxTableWidth < 1.08){
# #         # if table can fit by reducing font size by 1, do that instead of
# #         #   wrapping headers.
# #         wBody2=HBWidths*0.92
# #         newFontSize=newFontSize-1
# #     }else{
# #         wBody2=wBody
# #         wHeader2=wHeader
# #         indsH2=intersect(indsH,letterOnlyInds)
# #         for(i in indsH2){
# #             wBody2[i]=max(wBody[i],wHeader[i]+CP)
# #         }
# #
# #         indsH2=setdiff(indsH,letterOnlyInds)
# #         for(i in indsH2){
# #             #wrap headers
# #             ratio1=breakRatio(header1[i])
# #             # the closer ratio1 is to 1, the more likely ratio1*1.3 > 1
# #             #wHeader2[i]=wHeader[i]*ratio1*1.3
# #             wHeader2[i]=min(wHeader[i]*ratio1 + CP, wHeader[i]*0.9)
# #             # 1.3 is to give more room
# #             wBody2[i]=max(wBody[i],wHeader2[i])
# #         }
# #
# #         #wrap rowHeader if they are multi-cells
# #         if(sum(wBody2)>maxTableWidth){
# #             for(i in rowHeaderInd){
# #                 d1=sum(wBody2)-maxTableWidth
# #                 c1=nRowPerRowHeader[i]
# #                 if(c1>1){
# #                     wBody2[i]=wBody2[i]/c1 + max(wBody2[i]/c1-d1,0)
# #                     wBody2[i]=max(wBody2[i],wHeader2[i])
# #                 }
# #             }
# #         }
# #
# #         # reduce font size to minFontSize
# #         if(sum(wBody2)>maxTableWidth){
# #             fsr=sum(wBody2)/maxTableWidth - 1
# #             # character length reduces by 8% per font size
# #             fsr=min(ceiling(fsr/0.08), newFontSize-minFontSize)
# #             wBody2=wBody2*(1-fsr*0.08)
# #             wHeader2=wHeader2*(1-fsr*0.08)
# #             newFontSize=newFontSize-fsr
# #         }
# #
# #         # further wrap the header
# #         if(sum(wBody2)>maxTableWidth){
# #             wBody=wBody*(1-fsr*0.08)
# #
# #             wRatios3=(wHeader2-wBody2)/wBody2
# #             indsH3=which(wRatios3>0)
# #             indsH3=setdiff(indsH3,letterOnlyInds)
# #             for(i in indsH3){
# #                 #wrap headers
# #                 ratio1=breakRatio(header1[i],3)
# #                 # the closer ratio1 is to 1, the more likely ratio1*1.3 > 1
# #                 #wHeader2[i]=wHeader[i]*ratio1*1.3
# #                 wHeader2[i]=min(wHeader[i]*ratio1 + CP, wHeader[i]*0.9)
# #                 # 1.3 is to give more room
# #                 wBody2[i]=max(wBody[i],wHeader2[i])
# #             }
# #
# #         }
# #
# #         # wrap the table body, staring from
# #         #   the longest table columns.
# #         bLenInds=order(wBody2,decreasing = TRUE)
# #         i=1
# #         while(sum(wBody2)>maxTableWidth && i<=length(bLenInds)){
# #             j=bLenInds[i]
# #             if(!(j %in% rowHeaderInd)){
# #                 d1=sum(wBody2)-maxTableWidth
# #                 # set wBody2[j] exactly by half may cause wrapping into three lines
# #                 #   therefore setting a value slightly smaller than 2
# #                 c1=ifelse(wBody2[j]/2<d1,1.95,2)
# #                 wBody2[j]=wBody2[j]/c1 + max(wBody2[j]/c1-d1,0)
# #                 wBody2[j]=max(wBody2[j],wHeader2[j])
# #             }
# #
# #             i=i+1
# #         }
# #
# #         # while(sum(wBody2)>maxTableWidth && i<=length(bLenInds)){
# #         #     j=bLenInds[i]
# #         #     d1=sum(wBody2)-maxTableWidth
# #         #     #c1=ifelse(d1/wBody2[i]>0.5 && i <= rowHeaderInd,2.95,1.95)
# #         #     wBody2[j]=wBody2[j]/c1 + max(wBody2[j]/c1-d1,0)
# #         #     wBody2[j]=max(wBody2[j],wHeader[j])
# #         #     i=i+1
# #         # }
# #
# #     }
# #
# #     list(widths=wBody2,fs=newFontSize)
# # }
#
