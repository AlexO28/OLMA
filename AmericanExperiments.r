library(data.table)

GetAllData <- function(storagepath) {
  files <- list.files(storagepath)
  #first <- TRUE
  rtabs <- list()
  count <- 1
  for (afile in files) {
    print(afile)
	tab <- fread(paste(storagepath, afile, sep = '\\'), sep = ';')
	tab[, mydate :=  as.POSIXct(paste(DATE, TIME), format = '%Y%m%d %H:%M:%S')]
	rtabs[[count]] <- tab
	count <- count + 1
  }
rbindlist(rtabs)
}