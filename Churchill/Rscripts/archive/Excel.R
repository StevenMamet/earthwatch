# library(rJava)
library("xlsx")
library(XLConnect, pos = 4)

setwd("~/Dropbox/School/School/Postdoc/Earthwatch/EWChurchill2000to2013/Snow/2002")

rm(list=ls())

file.names <- list.files(pattern='*.xls')
sheet.names <- getSheets(loadWorkbook('Name.xls'))
e.names <- paste0(rep('v', 28), c(1:28))
data.1 <- data.frame(matrix(rep(NA,length(e.names)),
                            ncol = length(e.names)))

names(data.1) <- e.names

for (i in 1:length(file.names)) {
  wb <- loadWorkbook(file.names[i])
  for (j in 1:length(sheet.names)) {
    ss <- readWorksheet(wb, sheet.names[j], startCol = 2, header = TRUE)
    condition <- rep(sheet.names[j], nrow(ss))
    sub.id <- rep(file.names[i], nrow(ss))
    s.frame <- seq(1:nrow(ss))
    df.1 <- data.frame(sub.id, condition, s.frame, ss)
    names(df.1) <- e.names
    data.1 <- rbind(data.1, df.1)
    rm(ss, condition, s.frame, sub.id, df.1)
  }
  rm(wb)
}