PrepareDataForBaskets <- function(storagepath = "\\\\192.168.1.204\\share\\People\\Алексей\\") {
  dirlist <- list.dirs(paste(storagepath, 'Data', sep = "\\"), recursive = FALSE, full.names = FALSE)
  dirlist <- unlist(strsplit(dirlist, '@'))
  dirlist <- cbind(dirlist[seq(1, length(dirlist), 2)], dirlist[seq(2, length(dirlist), 2)])
  dirlist <- dirlist[dirlist[,2] == "TQBR", ]
  dirlist <- paste(dirlist[, 1], dirlist[, 2], sep = "@")
  for (instr in dirlist) {
    tab <- GetCleanData("spot", instr, datestart = as.Date("2015-01-01"),
                        dateend = as.Date("2016-12-31"), candletype = "Data", timelen = 1,
                        storage.path = storagepath)
    write.table(tab, paste0(storagepath, "DataRussianSpreads\\", instr, ".csv"),
                sep = ";", row.names = FALSE)
  }
}
