# functions for load candles data

# create empty data-frame
CreateCandle <- function() {
  data.frame(
    Time=as.POSIXct(numeric(), origin=origin, tz="UTC"),
    High=numeric(),
    Open=numeric(),
    Low=numeric(),
    Close=numeric(),
    Volume=numeric(),
    Date=as.Date(numeric(), origin=origin),
    stringsAsFactors = FALSE
  )
}

# load candles from file
LoadCandlesFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL) {
	df <- read.csv2(file, as.is=T)
	df$Time <- as.POSIXct(df$Time, origin=origin, tz="UTC")
	df$Date <- as.Date(df$Time, origin=origin)
	if(only.main.session)
	{
		# only main session = T => use 10:00-18:45
		dates <- df$Date
		df <- df[dates + hm("10:00") <= df$Time &
						 df$Time < dates + hm("18:45"),  ]
	}
	else if(!is.null(start.time)){
		# only main session = F and start.time != NULL => use [start.time - end.time]
		dates <- df$Date
		df <- df[dates + hm(start.time) <= df$Time &
						 	df$Time < dates + hm(end.time),  ]
	}
	df
}

# take some data from storage [one day], use start.time as "hh:mm"
LoadCandlesDay <- function(instrument, date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "FINAM")) {
	date <- as.character(date, format="%Y_%m_%d")
	dir <- file.path(storage.path, candle.type, instrument)
	file <- list.files(dir, pattern=date)
	df <- CreateCandle()
	if(length(file)==0) return(df)
	df <- LoadCandlesFile(file.path(dir,file), only.main.session, start.time, end.time)
}


# download candle from local storage [start.date,end.date]
LoadCandles <- function(instrument, start.date, end.date, candle.type="1m", only.main.session=F, start.time = NULL, end.time = NULL, storage.path=file.path("//192.168.1.12","historical_data", "FINAM")) {
	if(!is.Date(start.date))
		start.date <- as.Date(start.date, origin=origin)
	if(!is.Date(end.date))
		end.date <- as.Date(end.date, origin=origin)
	as.data.frame(
		do.call(
			rbind, lapply(as.Date(start.date:end.date, origin=origin), function(date) {
					LoadCandlesDay(instrument, date, candle.type, only.main.session, start.time, end.time, storage.path)
				})
		)
	)
}


# shrink df to date-interval
ShrinkCandles <- function(data, interval) {
  start <- ymd(interval[1])
  end <- ymd(interval[2])
  int  <- new_interval(start, end)
  dt <-  data$Date
  data[dt %within% int, ]
}

# get candles from resourses-folder
LoadCandlesResources <- function(name, only.main.session = F) {
	file <- file.path(getwd(), "..", "resources", "historical_data", paste0(name,".csv"))
	LoadCandlesFile(file, only.main.session)
}
