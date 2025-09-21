# functions for load bid-ask data
SetGlobalOptions <- function() {
  options(stringsAsFactors = FALSE)
  options(digits.secs = 3)
  Sys.setenv(tz = "GMT")
}

LoadBpFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL, fast = FALSE) {
  #df <- read.csv2(file, as.is = TRUE, na.strings="-1")
  df <- fread(file, na.strings = "-1", drop = 4, dec = ',')
  df[, Time := fastPOSIXct(Time, tz = "GMT")]
  date0 <- as.Date(df$Time[1])
  df[, Date := rep(date0, nrow(df))]
  if (fast) {return(df)}
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
    df <- as.data.frame(df)
    print(start.time)
    print(end.time)
    df <- df[dates + hms(start.time) <= df$Time &
               df$Time <= dates + hms(end.time),  ]
    df <- as.data.table(df)
  }
  df[, Bid := as.numeric(sub(',', '.', Bid))]
  df[, Ask := as.numeric(sub(',', '.', Ask))]
  df[, Close := (Bid + Ask)/2]
df
}

# take some bid-ask data from storage [one day], use hh:mm for specific time period
LoadBpDay <- function(instrument, date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp"),
                      fast = FALSE) {
  dir <- file.path(storage.path, candle.type, instrument)
  file <- list.files(dir, pattern=as.character(date, "%Y_%m_%d"))

  print(dir)
  print(file)

  if(length(file)==0)print(paste0(instrument,"!: ",as.character(date, "%Y_%m_%d")," not found"))
  if (length(file) > 0) {
    ticks <- LoadBpFile(file.path(dir, file), only.main.session, start.time, end.time, fast = fast)
  } else {
    ticks <- CreateBp()
  }
  ticks
}

# download bid-ask data from local storage [start.date,end.date]
LoadBp <- function(instrument, start.date, end.date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp"),
                  fast = FALSE) {
	SetGlobalOptions()
  start.date <- as.Date(start.date, origin=origin)
  end.date <- as.Date(end.date, origin=origin)
  as.data.table(
    do.call(
      rbind, lapply(as.Date(start.date:end.date, origin=origin), function(date) {
        LoadBpDay(instrument, date, candle.type, only.main.session, start.time, end.time, storage.path, fast = fast)
      })
    )
  )
}
CreateBp <- function() {
  data.frame(
    Time=as.POSIXct(numeric(), origin=origin),
    Bid=numeric(),
    Ask=numeric(),
    Date=as.Date(numeric(), origin=origin),
    Close = numeric(),
    stringsAsFactors = FALSE
  )
}


LoadBpChar <- function(instrument, date, storage, subdirs) {
	SetGlobalOptions()
  tab <- rbind(c(), c())
  for (j in 1:length(subdirs)) {
    mypath <- file.path(storage, subdirs[j], instrument)
    print(mypath)
    file <- list.files(mypath, pattern=as.character(date, "%Y_%m_%d"))
    print(file)
    tab <- rbind(tab, read.table(file.path(mypath, file), header = TRUE, sep = ";"))
    tab$Time <- as.POSIXct(tab$Time)
  }
tab
}
LoadBpVarInstrs <- function(storage, expinfo, candletype) {
	SetGlobalOptions()
  tab <- CreateBp()
  for (j in 1:nrow(expinfo)) {
    tab <- rbind(tab, LoadBp(start.date = expinfo$start[j],
                             end.date = expinfo$end[j],
                             instrument = expinfo$fullname[j],
                             storage.path = storage,
                             candle.type = candletype))
  }
tab
}
