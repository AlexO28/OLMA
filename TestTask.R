library(data.table)

ModifyTheFile <- function(src = "D:\\TestTask\\", file0) {
  dat <- fread(paste0(src, file0))
  dat <- dat[, c(2, 4, 7, 8, 9), with = FALSE]
  print(head(dat))
  write.table(dat, paste0(src, file0), quote = FALSE, sep = ";", row.names = FALSE)
}

#ModifyTheFile(file0 = "traderAdol.csv")
ModifyTheFile(file0 = "traderAfut.csv")
ModifyTheFile(file0 = "traderIdol.csv")
ModifyTheFile(file0 = "traderIfut.csv")
ModifyTheFile(file0 = "traderNdol.csv")
ModifyTheFile(file0 = "traderNfut.csv")
ModifyTheFile(file0 = "traderSdol.csv")
ModifyTheFile(file0 = "traderSfut.csv")

