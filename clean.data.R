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
  #we check whether we need to get data from Finam (from 2017-03-20 to 2017-03-28)
  finamdate1 <- as.Date("2017-03-20")
  finamdate2 <- as.Date("2017-03-28")
  tab <- as.data.table(tab)
  if ((datestart <= finamdate2) & (dateend >= finamdate1)) {
    print(names(tab))
    print("old data")
    print(unique(tab$Date))
    tab <- tab[(Date < finamdate1) | (Date > finamdate2), ]
    print("parsed old data")
    print(unique(tab$Date))
    print("downloading finam data")
    if (substr(instr, 1, 2) == "SI") {
      tabfinam <- fread("\\\\192.168.1.204\\share\\People\\Алексей\\FinamData\\SPFB.Si-6.17_170320_170328.txt", sep = ';')
    } else if (substr(instr, 1, 3) == "USD") {
      tabfinam <- fread("\\\\192.168.1.204\\share\\People\\Алексей\\FinamData\\USD000UTSTOM_170320_170328.txt", sep = ';')
    } else {
      stop("Data from Finam is not downloaded! Please download Finam data!")
    }
    names(tabfinam) <- c("DATE", "TIME", "OPEN", "LOW", "HIGH", "CLOSE", "VOL")
    tabfinam[, Time := fastPOSIXct(strptime(paste(DATE, TIME), format = "%Y%m%d %H:%M:%S"))]
    tabfinam[, Date := as.Date(Time)]
    tabfinam <- data.table(Time = tabfinam$Time, Bid = tabfinam$CLOSE, Ask = tabfinam$CLOSE, Date = as.Date(tabfinam$Time), Close = tabfinam$CLOSE)
    tab <- rbind(tab, tabfinam)
    setkey(tab, "Time")
  }
  tab <- as.data.frame(tab)
	names(tab) <- tolower(names(tab))
	names(tab)[1] <- "Time"
	mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "23:40:00"), tz = "GMT")
	tab <- tab[mask>0, ]
	print("mask chosen")
	ind1 <- Purify2(tab, 15*timelen, strname = "")
	if (length(ind1) > 0) {tab <- tab[-ind1, ]}
tab
}

# several functions for cleaning data


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


# TO DO
FullJoinDf <- function(list.df) {
	# create df (join from list) with equal points per day (use NA for missed values)

}


# return same list of dfs but with dropped dates
MergeByDate <- function(list.df) {
	# drop missed dates (use %in%)
	# each df has Date as key-value
	dates <- NULL
	for (i in seq_along(list.df)) {
	  if (is.null(list.df[[i]]$Date)) {
	  	stop(paste0("data.frame N)", i, "does not have Date column!"))
	  }
	  datescur <- unique(list.df[[i]]$Date)
	  if (i == 1) {
	    dates <- datescur
	  } else {
	    dates <- dates[dates %in% datescur]
	  }
	}
	for (i in 1:length(list.df)) {
		list.df[[i]] <- list.df[[i]][list.df[[i]]$Date %in% dates, ]
	}
list.df
}

# remove outliers from vector
CleanDf <- function(indicator, sd.coeffs=3, roll.width=60) {
	x <- RollMedian(indicator,roll.width)
	w <- RollSd(indicator,roll.width)
	up.boundary <- x + sd.coeffs*w
	down.boundary <- x - sd.coeffs*w
  up.boundary[is.na(up.boundary)] <- x[is.na(up.boundary)]
  down.boundary[is.na(down.boundary)] <- x[is.na(down.boundary)]
  index <- (indicator>up.boundary) & (indicator<down.boundary)
	indicator[indicator>up.boundary] <- up.boundary[indicator>up.boundary]
	indicator[indicator<down.boundary] <- down.boundary[indicator<down.boundary]
indicator
}
