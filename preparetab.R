PrepareTab <- function(tab) {
	names(tab) <- tolower(names(tab))
  if (!(tolower("Bid") %in% names(tab)) | !(tolower("Ask") %in% names(tab))) {
    if (tolower("Close") %in% names(tab)) {
    	ind <- which(names(tab) == tolower("Close"))
      tab$Bid <- tab[, ind]
      tab$Ask <- tab[, ind]
    } else {
      stop("Unknown format of tab! Can not find neither Bid nor Ask nor Close!")
    }
  }
  if (!(tolower("Close") %in% names(tab))) {
    tab$close <- (tab$bid + tab$ask)/2
  }
	names(tab) <- tolower(names(tab))
tab
}
Formatize <- function(tab) {
  times <- strptime(paste(tab$X.DATE., tab$X.TIME), format = "%Y%m%d %H:%M:%S", tz = "UTC")
  closes <- tab$X.CLOSE.
  data.frame(time = times, close = closes, date = as.Date(times))
}
FillDaysTillExpir <- function(tab, startdate, enddate, num) {
  tab$daystillexpir[tab$date > startdate & tab$date < enddate] <-
    as.numeric(enddate -
                 tab$date[tab$date > startdate & tab$date < enddate])
  tab$periodnum[tab$date > startdate & tab$date < enddate] <- num
  tab
}
MainInitialization <- function(tab, thename) {
  tab$time <- tab[, thename]
  tab$date <- as.Date(tab$time)
  tab <- Initialization(tab, 1)
  tab <- tab[tab$daystillexpir > 5 & tab$daystillexpir < 85, ]
  temptab <- aggregate(tab$daystillexpir, by = list(tab$periodnum), FUN = "max")
  tab$maxday <- temptab[match(tab$periodnum, temptab[, 1]), 2]
  na.omit(tab)
}
Initialization <- function(tab, num) {
  tab <- FillDaysTillExpir(tab, as.Date("2013-12-16"), as.Date("2014-03-17"), num)
  tab <- FillDaysTillExpir(tab, as.Date("2014-03-17"), as.Date("2014-06-16"), num+1)
  tab <- FillDaysTillExpir(tab, as.Date("2014-06-16"), as.Date("2014-09-15"), num+2)
  tab <- FillDaysTillExpir(tab, as.Date("2014-09-15"), as.Date("2014-12-15"), num+3)
  tab <- FillDaysTillExpir(tab, as.Date("2014-12-15"), as.Date("2015-03-16"), num+4)
  tab <- FillDaysTillExpir(tab, as.Date("2015-03-16"), as.Date("2015-06-15"), num+5)
  tab <- FillDaysTillExpir(tab, as.Date("2015-06-15"), as.Date("2015-09-15"), num+6)
  tab <- FillDaysTillExpir(tab, as.Date("2015-09-16"), as.Date("2015-12-15"), num+7)
  tab <- FillDaysTillExpir(tab, as.Date("2015-12-16"), as.Date("2016-03-15"), num+8)
  tab
}

