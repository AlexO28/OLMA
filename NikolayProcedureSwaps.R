GetAllSwaps <- function(file, datestart, dateend, nextdate) {
  tabswaps <- LoadTradesFromMetroplex(file)
  tabswaps[, Date := as.Date(Time)]
  tabswaps[, hour := as.POSIXlt(Time)$hour]
  tabswaps <- tabswaps[(hour >= 10) & (hour <= 12), ]
  restab <- tabswaps[, sum(Price*Volume)/sum(Volume), by = Date]
  names(restab) <- c("Date", "swapval")
  restab <- restab[Date >= datestart & Date <= dateend, ]
  restab$newdate <- c(restab$Date[2:nrow(restab)], nextdate)
  restab$diffdate <- as.numeric(restab$newdate) - as.numeric(restab$Date)
restab
}
#requires DollarModelOnSwaps.R
GetAggrQuotesInfo <- function(tab) {
  restab <- tab[, median(newstavka), by = Date]
  names(restab) <- c("Date", "stavka")
  restab[, annstavka := NA]

  restab <- FillAnnStavka(restab, as.Date("2015-09-01"), as.Date("2015-09-15"), as.Date("2015-12-14"), as.Date("2015-12-15"), tab)
  restab <- FillAnnStavka(restab, as.Date("2015-12-01"), as.Date("2015-12-15"), as.Date("2016-03-14"), as.Date("2016-03-15"), tab)
  restab <- FillAnnStavka(restab, as.Date("2016-03-01"), as.Date("2016-03-15"), as.Date("2016-06-14"), as.Date("2016-06-15"), tab)
  restab <- FillAnnStavka(restab, as.Date("2016-06-01"), as.Date("2016-06-15"), as.Date("2016-09-14"), as.Date("2016-09-15"), tab)
  restab <- FillAnnStavka(restab, as.Date("2016-09-01"), as.Date("2016-09-15"), as.Date("2016-12-14"), as.Date("2016-12-15"), tab)
restab
}
#datestart --- start of all dates
#datestarteff --- we need data from this date
#dateend --- end of all dates
#critdate --- end of expiration period
FillAnnStavka <- function(restab, datestart, datestarteff, dateend, critdate, tab) {
  restab <- as.data.table(as.data.frame(restab))
  dolval <- FirstValByDate(tab, datestart)
  print(dolval)
  restab$annstavka[restab$Date >= datestarteff & restab$Date <= dateend] <-
    CalculateAnnStavka(restab$stavka[restab$Date >= datestarteff & restab$Date <= dateend], datestarteff, critdate, dolval)
restab
}
FirstValByDate <- function(tab, adate) {
  tab[Date >= adate, dol.close][1]
}
FillAnnStavkaForSwaps <- function(tabswaps, tab, datestart, datestarteff, dateend) {
  tabswaps <- as.data.table(as.data.frame(tabswaps))
  dolval <- FirstValByDate(tab, datestart)
  print(dolval)
  tabswaps$annswapval[tabswaps$Date >= datestarteff & tabswaps$Date <= dateend] <-
    tabswaps$swapval[tabswaps$Date >= datestarteff & tabswaps$Date <= dateend]*100*365/dolval
tabswaps
}
FillAnnStavkaForSwapsMain <- function(tabswaps, tab) {
  tabswaps <- as.data.table(as.data.frame(tabswaps))
  tabswaps <- FillAnnStavkaForSwaps(tabswaps, tab, as.Date("2015-09-01"), as.Date("2015-09-15"), as.Date("2015-12-14"))
  tabswaps <- FillAnnStavkaForSwaps(tabswaps, tab, as.Date("2015-12-01"), as.Date("2015-12-15"), as.Date("2016-03-14"))
  tabswaps <- FillAnnStavkaForSwaps(tabswaps, tab, as.Date("2016-03-01"), as.Date("2016-03-15"), as.Date("2016-06-14"))
  tabswaps <- FillAnnStavkaForSwaps(tabswaps, tab, as.Date("2016-06-01"), as.Date("2016-06-15"), as.Date("2016-09-14"))
  tabswaps <- FillAnnStavkaForSwaps(tabswaps, tab, as.Date("2016-09-01"), as.Date("2016-09-15"), as.Date("2016-12-14"))
tabswaps
}
PlotTwoStavkas <- function(datestart, dateend, restab, tabswaps) {
  restab <- as.data.table(as.data.frame(restab))
  tabswaps <- as.data.table(as.data.frame(tabswaps))
  restab <- restab[Date >= datestart & Date <= dateend, ]
  tabswaps <- tabswaps[Date >= datestart & Date <= dateend, ]
  layout(matrix(1:2))
  plot(restab$Date, restab$annstavka)
  plot(tabswaps$Date, tabswaps$annswapval)
}
