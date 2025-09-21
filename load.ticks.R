# functions for loading trades (ticks)
LoadTradesFromMetroplex <- function(file, modify=FALSE) {
  SetGlobalOptions()
  tab <- fread(file, header = TRUE, sep = ";", dec = ",")
  #tab <- data.frame(Time = strptime(tab$Date, format =  "%d.%m.%Y %H:%M:%S", tz = "UTC"),
  #									Price = tab$Price, Volume = tab$Volume, Dir = as.character(tab$Side), Instrument = tab$Security, Comment = tab$Comment)
  tab[, Time := fastPOSIXct(Time)]
  tab[, TimeNum := as.numeric(Time)]

  setkey(tab, "TimeNum")
  if (modify) {
    tab <- ModifyTrades(tab)
  }
#data.frame(Time = tab$Time, Dir = tab$Dir, Price = tab$Price, Volume = tab$Volume,
#					 Comment = tab$Comment, Instrument = tab$Instrument, Date = as.Date(tab$Time, origin = origin), stringsAsFactors = FALSE)
tab
}

ModifyTrades <- function(tab) {
	tabprice <- aggregate(tab$Price*tab$Volume, list(Time = tab$Time, Instrument = tab$Instrument, Dir = tab$Dir, Comment = tab$Comment), sum)
	tabvolume <- aggregate(tab$Volume, list(Time = tab$Time, Instrument = tab$Instrument, Dir = tab$Dir, Comment = tab$Comment), sum)
	names(tabprice)[5] <- "Price"
	names(tabvolume)[5] <- "Volume"
  tab <- cbind(tabprice, tabvolume)
	tab$Price <- tab$Price/tab$Volume
tab
}

# create empty trade
CreateTrade <- function() {
	data.frame(
		Time=as.POSIXct(numeric(), origin=origin),
		Price=numeric(),
		Volume=numeric(),
		Direction = character()
	)
}

# load ticks from file
LoadTradesFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL) {
  df <- fread(file, dec=".", na.strings="-1")
  df[, Time := fastPOSIXct(Time)]
}

# take some trade from storage [one day], use start.time as "hh:mm:ss"
LoadTradesDay <- function(instrument, date,  only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA", "ticks"), verbose=F) {
  dir <- file.path(storage.path, instrument)
  char.date <- as.character(date, "%Y_%m_%d")
  file <- list.files(dir, pattern=char.date)
  if(length(file)==0) {
    ticks <- CreateTrade()
    if(verbose && wday(date) %in% 2:6) print(paste0(instrument,": ", char.date, " is not found"))
  }
  else {
    ticks <- LoadTradesFile(file.path(dir, file), only.main.session, start.time, end.time)
    if(verbose) print(paste0(instrument, ": ", char.date, " ", nrow(ticks), " trades"))
  }
  ticks
}

# load trades from storage
# NB!! start.time has to be in h:m:s
LoadTrades <- function(instrument, start.date, end.date, only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","ticks"), verbose=F) {
  SetGlobalOptions()
  dates <- seq(as.Date(start.date, origin=origin), to = as.Date(end.date, origin=origin), by = "days")
  res <- CreateTrade()
  for (date in dates) {
    res <- rbind(res, LoadTradesDay(instrument, as.Date(date, origin), only.main.session, start.time, end.time, storage.path, verbose))
  }
  res[, Date := as.Date(Time)]
  res
}
