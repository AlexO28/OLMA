empty.candle.frame <- function() {
  data.frame(
    Time=numeric(),
    High=numeric(),
    Open=numeric(),
    Low=numeric(),
    Close=numeric(),
    Volume=numeric()
  )
}

# load candles from file
load.candles <- function(file, only.main.session = F) {
	df <- read.csv2(file, as.is=T)
	df$Time <- as.POSIXct(df$Time, tz = "UTC")
	if(only.main.session)
	{
		dates <- as.Date(df$Time, tz="UTC")
		df <- df[dates + hm("10:00") <= df$Time &
						 df$Time <dates + hm("18:45"),  ]
	}
	df
}

# get candles from resourses
load.candles.from.resources <- function(name, only.main.session = F) {
  file <- file.path(getwd(), "..", "resources", "historical_data", paste0(name, ".csv"))
  load.candles(file, only.main.session)
}

# download candle from local storage [start.date,end.date]
load.candles.from.storage <- function(storage.path, instrument, start, end=start, storage.type="FINAM", candle.type="1m", only.main.session=F) {

	as.data.frame(do.call(rbind, lapply(ymd(start):ymd(end), function(date) {
					load.candles.storage.day(storage.path, instrument, date, candle.type, only.main.session=only.main.session)
				})))
}

# lets take some data from storage [one day], date = yyyy-mm-dd
load.candles.storage.day <- function(storage.path, instrument, date, candle.type="1m", only.main.session=F) {
print("starting")
	dir <- file.path(storage.path, candle.type, instrument)
print(dir)
	file <- list.files(dir, pattern=date)
	df <- empty.candle.frame()
	if(file != "") {
		df <- load.candles(file, only.main.session)
	}
	df
}

# shrink df to date-interval
shrink.candles <- function(data, interval) {
  start <- ymd(interval[1])
  end <- ymd(interval[2])
  int  <- new_interval(start, end)
  dt <-  as.Date(data$Time, tz="UTC")
  data[dt %within% int, ]
}

# load instrument parametrs
load.info <- function(instrument) {
  file <- file.path(getwd(), "..", "resources", "info.txt")
  data <- read.csv(file,sep="\t",skip=2,dec=",",stringsAsFactors = FALSE)
  plaza.match <- sapply(grepl, X = data$plaza, instrument)
  plaza.match[is.na(plaza.match)]<-F

  finam.match <- sapply(grepl, X = data$finam, instrument)
  finam.match[is.na(finam.match)]<-F
  index <- which(plaza.match | finam.match)
  if(length(index) == 1) {
    info <- data[index,]
    info$point.price <- info$stepprice/info$stepsize
    return(info)
  }
  return (NA)
}



# return df [df$Time or df$time] which contains specific date only
day.mask <- function(df, date) {
	d = (df$Time)
	if(is.null(d)) {
		d = df$time
	}
	df$dt = as.POSIXct(d, origin="1970-01-01", tz="UTC")
	df[as.Date(df$dt) == as.Date(date), ]
}