ArbitrageTesterForSecur <- function(datestart, dateend, candletype, storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\",
                            curspotname, curfutname, spread, num, critdate, nextdate) {
  mymod <- GetModByInstr(curspotname)
  tab <- FillStavkaTab(as.Date(datestart) - 2*num, dateend, spotsec = curspotname, futsec = curfutname, 
                       candletype = candletype, mod = mymod, expinfo = NA, type = "spot", num1 = 1, num2 = mymod)
  tab <- tab[Date >= datestart, ]
  datestab <- tab[, length(dol.bid), by = Date]
  len <- mean(datestab$V1)
  print(len)
  #step 0: identify the model
  setkey(tab, "Time")
  tab <- PrepareModel(tab, critdate, nextdate)
  tab <- FillAvStavkas(tab) 
  tab <- FillMean(tab, "avstavka1", num = num*len, mode = "exponential")
  tab <- MakeReal(tab, "avstavka1exp")
  tab$lintrend <- NULL
  tab <- na.omit(tab)
  tab[, model := stavka - avstavka1expreal]
  #step 1: get position analogs
  tab[, pos := trunc(model/spread)]
  #step 2: get moments of position changes
  tab[, posdiff := c(0, diff(pos))]
  timechanges <- which(abs(tab$posdiff) != 0 )
  #print(head(tab$posdiff))
  #print(summary(tab$posdiff))
#print(head(timechanges))
#print(summary(timechanges))
  initpos <- tab$pos[1] 
  spreadopport <- 0
  for (ind in timechanges) {
    initposdiff <- tab$posdiff[ind]
    if ((initpos > 0 & initposdiff < 0) | (initpos < 0 & initposdiff > 0)) {
      spreadopport <- spreadopport + min(abs(initposdiff), abs(initpos))
    }
#print(c(initpos, initposdiff, spreadopport)) 
    initpos <- initpos + initposdiff
  }
list(opportunity = spreadopport, remaining = abs(initpos), tab = tab)
}
#############From Old Test Files
GetModByInstr <- function(instrname) {
  if (instrname == "USD000UTSTOM@CETS") {
    return(1000)
  } else if (instrname == "SBER@TQBR" | instrname == "GAZP@TQBR") {
    return(100)
  } else if (instrname == "LKOH@TQBR") {
    return(10)
  } else if (instrname == "VTBR@TQBR") {
    return(100000)
  } else {
    return(NA)
  }
}
#############From Research.R
FillLinearTrend <- function(tab, critdate, alpha1, alpha2) {
  tab[, daysdiff := as.numeric(critdate) - as.numeric(si.date)]
  tab[, lintrend := alpha1*daysdiff + alpha2]
}
DefineDatesStruct <- function(tab, datecrit, datenext) {
  dates <- unique(tab$Date)
  diffs <- as.numeric(datecrit) - as.numeric(dates)
  datestruct <- data.table(dates = dates, diffs = diffs)
  datestruct$newdiffs <- c(datestruct$diffs[2:nrow(datestruct)],
                           as.numeric(datecrit) - as.numeric(datenext))
setkey(na.omit(datestruct), "dates")
}
FillModelInfo <- function(tab, datestruct) {
  tabnew <- tab
  setkey(tabnew, "Date")
tabnew[datestruct]
}
PrepareModel <- function(tab, critdate, nextdate) {
  tab[, Date := si.date]
  tab <- FillLinearTrend(tab, critdate, NA, NA)
  datestruct <- DefineDatesStruct(tab, critdate, nextdate)
  tab <- FillModelInfo(tab, datestruct)
}
FillAvStavkas <- function(traintab) {
  traintab[, avstavka1 := stavka/newdiffs]
  traintab[, avstavka2 := stavka/(newdiffs*dol.close)]
}
FillMean <- function(tab, colname, num, mode = "simple") {
  tab <- as.data.table(as.data.frame(tab))
  if (mode == "simple") {
    tab[, paste0(colname, "simpl") := SMA(tab[, colname, with = FALSE], num), with = FALSE]
  } else if (mode == "exponential") {
    tab[, paste0(colname, "exp") := EMA(tab[, colname, with = FALSE], num), with = FALSE]
  }
tab
}
MakeReal <- function(tab, colname, withdollar = FALSE) {
  tab <- as.data.table(as.data.frame(tab))
  if (!withdollar) {
    tab[, paste0(colname, "real") := tab[, colname, with = FALSE]*tab$newdiffs, with = FALSE]
  } else {
    tab[, paste0(colname, "real") := tab[, colname, with = FALSE]*tab$newdiffs*tab$dol.close, with = FALSE]
  }
tab
}
#############From Old Library Files
FillStavkaTab <- function(datestart, dateend, spotsec = "USD000UTSTOM@CETS", futsec = "SIH6@FORTS",
                          candletype = "1sVol50", mod = 1000, expinfo = NA, type = "spot", num1 = num1, num2 = num2) {
  if ((candletype == "1mVol50") | (candletype == "Data")) {
    timelen <- 1
  } else {
    timelen <- 60
  }
  tabspot <- GetCleanData("spot", spotsec, datestart = datestart, dateend = dateend,
                          storage.path = "\\\\192.168.1.204\\share\\People\\Алексей", candletype = candletype, timelen = timelen)
  if (type == "fut") {
    tabfut <- GetCleanData("fut", "SI", datestart = datestart, dateend = dateend,
                         storage.path = "\\\\192.168.1.204\\share\\People\\Алексей", candletype = candletype, timelen = timelen, expinfo = expinfo)
  } else if (type == "spot") {
    tabfut <- GetCleanData("spot", futsec, datestart = datestart, dateend = dateend,
                           storage.path = "\\\\192.168.1.204\\share\\People\\Алексей", candletype = candletype, timelen = timelen, expinfo = expinfo)
  }
  tab <- InnerMergeDf(list(dol = tabspot, si = tabfut), "Time")
  tab[, stavkabid := (si.bid*num1 - dol.bid*num2)]
  tab[, stavkaask := (si.ask*num1 - dol.ask*num2)]
  tab[, stavka := (stavkabid +stavkaask)/2]
  tab[, Date := dol.date]
}

LoadBpFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL) {
  #df <- read.csv2(file, as.is = TRUE, na.strings="-1")
  df <- fread(file, na.strings = "-1", drop = 4, dec = ',')
  df[, Time := fastPOSIXct(Time, tz = "GMT")]
  date0 <- as.Date(df$Time[1])
  df[, Date := rep(date0, nrow(df))]
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
LoadBpDay <- function(instrument, date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp")) {
  dir <- file.path(storage.path, candle.type, instrument)
  file <- list.files(dir, pattern=as.character(date, "%Y_%m_%d"))
  if(length(file)==0)print(paste0(instrument,"!: ",as.character(date, "%Y_%m_%d")," not found"))
  if (length(file) > 0) {
    ticks <- LoadBpFile(file.path(dir, file), only.main.session, start.time, end.time)
  } else {
    ticks <- CreateBp()
  }
  ticks
}

# download bid-ask data from local storage [start.date,end.date]
LoadBp <- function(instrument, start.date, end.date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp")) {
	SetGlobalOptions()
  start.date <- as.Date(start.date, origin=origin)
  end.date <- as.Date(end.date, origin=origin)
  as.data.table(
    do.call(
      rbind, lapply(as.Date(start.date:end.date, origin=origin), function(date) {
        LoadBpDay(instrument, date, candle.type, only.main.session, start.time, end.time, storage.path)
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
GetCleanData <- function(type, instr, expinfo = NA, datestart = NA, dateend = NA, storage.path, candletype, timelen) {
  if (type == "spot") {
    tab <- as.data.frame(na.omit(LoadBp(instr, datestart, dateend, storage.path = storage.path, candle.type = candletype)))
  } else if (type == "fut") {
  	if (is.na(expinfo)) {
  	  stop("expinfo is not provided!")
  	}
  	expinfo$fullname <- paste0(instr, expinfo$shortname, "@FORTS")
  	tab <- as.data.frame(na.omit(LoadBpVarInstrs(storage = storage.path, candletype = candletype, expinfo = expinfo)))
  }
	names(tab) <- tolower(names(tab))
	names(tab)[1] <- "Time"
	mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "23:40:00"), tz = "GMT")
	tab <- tab[mask>0, ]
	print("mask chosen")
	ind1 <- Purify2(tab, 15*timelen, strname = "")
	if (length(ind1) > 0) {tab <- tab[-ind1, ]}
tab
}
# return merdged df
InnerMergeDf <- function(list.df, key){
	# key is the name of the key-value
	# return merdge df
  df.start <- as.data.table(list.df[[1]])
  if (is.null(df.start[, key])) {
    stop("data.frame N1 does not have the key column!")
  }
  setkey(df.start, "Time")
  if (length(list.df) > 1) {
    for (i in 2:length(list.df)) {
      print(i)
      df.next <- as.data.table(list.df[[i]])
      setkey(df.next, "Time")
      if (is.null(df.start[, key])) {
        stop(paste0("data.frame ", i, " does not have time column!"))
      }
#      ind.start <- df.start[, key] %in% df.next[, key]
#      ind.next <- df.next[, key] %in% df.start[, key]
#      df.start <- cbind(df.start[ind.start, ], df.next[ind.next, ])
       df.start <- df.start[df.next, nomatch = 0]
    }
  }
  i <- 1
  j <- 2
  while (j<=ncol(df.start)) {
    print(c(i, j))
    df.next <- list.df[[i]]
    modnames <- names(df.next)
    modnames <- modnames[modnames != key]
    for (aname in modnames) {
      names(df.start)[j] <- paste0(names(list.df[i]), ".", aname)
      j <- j + 1
    }
#    for (k in 1:ncol(df.next)) {
#      print(k)
#      names(df.start)[j] <- paste0(names(list.df[i]), ".", names(df.next)[k])
#      j <- j + 1
#    }
    i <- i + 1
  }

  df.start
}
MaskByTime <- function(df, start, end, close.every.day=T, days=NULL, tz="UTC") {
  # return mask-vector consists of {0,1,2} where
  # 0 - sleep, 1 - work, 2 - close position on this candle, bitch!
	if (length(start) != length(end)) {
		stop("Error in mask.by.time: length of start and end are not equal!")
	}
	if (is.null(df$Date)) {
		df$Date <- as.Date(df$Time, origin=origin)
	}
	if (is.null(days)) {
	  days <- unique(df$Date)
	}

  mask <- rep(0, nrow(df))
  for (i in 1:length(start)) {
  	start.val <- start[i]
  	end.val <- end[i]
  	if (is.na(tz)) {
    mask[(df$Time >= as.POSIXct(paste(df$Date, start.val))) &
    					 	(df$Time <= as.POSIXct(paste(df$Date, end.val))) &
    					 	(df$Date %in% days)] <- 1
  	} else {
  	  mask[(df$Time >= as.POSIXct(paste(df$Date, start.val), origin=origin, tz=tz)) &
  	               (df$Time <= as.POSIXct(paste(df$Date, end.val), origin=origin, tz=tz)) &
  	               (df$Date %in% days)] <- 1
  	}
  }

	if (close.every.day) {
			l <- lapply(split(mask, df$Date) ,function(x){
							ind <- max(which(x == 1))
							if(max(x)!=0)
								x[ind] <- 2
							x
						})
			mask <- unsplit(l,df$Date)
	}
  mask
}
Purify2 <- function(tab, len, strname) {
  strname1 <- paste0(strname, "bid")
  strname2 <- paste0(strname, "ask")
  print(strname1)
  print(strname2)

  mymax1 <- rollmaxr(tab[, strname1], len, FALSE)
  mymin1 <- -rollmaxr(-tab[, strname1], len, FALSE)
  mymax2 <- rollmaxr(tab[, strname2], len, FALSE)
  mymin2 <- -rollmaxr(-tab[, strname2], len, FALSE)
  signals <- (mymax1 == mymin1) & (mymax2 == mymin2)
  signals[1:len] <- FALSE
which(signals)
}
KillQuantiles <- function(tab, alpha, strname, len) {
  vec <- rollapplyr(tab[, strname], len, mean, partial = TRUE)
  diff <- tab[, strname] - vec
  quant1 <- as.numeric(quantile(diff, alpha))
  quant2 <- as.numeric(quantile(diff, 1-alpha))
tab[(diff >= quant1) & (diff <= quant2), ]
}
