context("test saveOutput")

skip_on_cran()

rm(list=ls())
library(wfr)
library(ggplot2)


df1=data.frame(A=c("a","a","b3"),
               B=c("b1","b2","b3"),
               C1=c(1001.123,58.04,32.01),
               C2=c(-0.00321, 0.0121, 0.325))
csvFile=tempfile(fileext = ".csv")
test_that("save csv file w/o formatting info", {
    saveOutput(df1, oFileName=csvFile,saveWorkspace = T,
               caption="testing table w/o formatting info")
    csvFile=file.path(dirname(csvFile),paste0('001.',basename(csvFile)))
    expect_identical(file.exists(csvFile),T)
})


pngFile=tempfile(fileext = ".png")
test_that("save png file", {
    saveOutput(qplot(1:10,1:10), oFileName=pngFile,
               caption="this is a testing plot")
    pngFile=file.path(dirname(pngFile),paste0('001.',basename(pngFile)))
    expect_identical(file.exists(pngFile),T)
})


colWidth = "2,1,1,1"
header = "A | A | C | C || A | A | C1 | C2"
footer = "A|Arkansas$~ref$|1|header
|| C1|Kansas$^ref$|x|header
|| a|Arizona|2|body"
rowHeaderInd = 2
csvFile=tempfile(fileext = ".csv")
test_that("save csv file w/ formatting info", {
    saveOutput(df1, oFileName = csvFile,
               caption = "testing table w/ formatting info",
               header = header, footer = footer, colWidth = colWidth,
               rowHeaderInd = rowHeaderInd)
    csvFile=file.path(dirname(csvFile),paste0('001.',basename(csvFile)))
    expect_identical(file.exists(csvFile),T)
})






