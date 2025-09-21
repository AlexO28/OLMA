library(zoo)

GlueFilesForFinam <- function(storage = "\\\\192.168.1.204\\share\\People\\Алексей") {
  filesaved <- paste(storage, "FinamData", "SPFB.MXI.saved.txt", sep = "\\")
  file <- paste(storage, "FinamData", "SPFB.MXI.txt", sep = "\\")
  GlueFilesForFinamServe(file, filesaved)
  filesaved <- paste(storage, "FinamData", "SPFB.RTS.saved.txt", sep = "\\")
  file <- paste(storage, "FinamData", "SPFB.RTS.txt", sep = "\\")
  GlueFilesForFinamServe(file, filesaved)
  filesaved <- paste(storage, "FinamData", "SPFB.Si.saved.txt", sep = "\\")
  file <- paste(storage, "FinamData", "SPFB.Si.txt", sep = "\\")
  GlueFilesForFinamServe(file, filesaved)
}
GlueFilesForFinamServe <- function(file, filesaved) {
  tab <- read.table(file, sep = ",", header = TRUE)
  tabsaved <- read.table(filesaved, sep = ",", header = TRUE)
  tab <- cbind(tabsaved, tab)
  write.table(tab, file, append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
}
Main <- function(winlen1 = 33, winlen2 = 3330, Type = "Z5", src, storage = "\\\\192.168.1.204\\share\\People\\Алексей") {
  inparams <- read.table(paste0(storage, "\\Results\\", "inparams.txt"), header = TRUE, sep = ",")
  winlen1 <- inparams$winlen1
  winlen2 <- inparams$winlen2
  print(c(winlen1, winlen2))
  tab <- GetData(Type = Type, src = src, storage = storage)
  storage <- paste(storage, "Results", sep = "\\")
  beta1 <- GetRollingBetas(tab, winlen1)
  beta2 <- GetRollingBetas(tab, winlen2)
  logfile <- paste(storage, "log.txt", sep = "\\")
  write.table(paste("Using data from", src), logfile, row.names = FALSE,  col.names = FALSE, append = TRUE)
  write.table(paste("Beta for", winlen2, "minutes is equal to", beta2[length(beta2)]), logfile, row.names = FALSE,  col.names = FALSE, append = TRUE)
  atime <- Sys.time()
  atime <- gsub(" ", "_", as.character(atime))
  atime <- gsub(":", "-", atime)
  file1 <- paste0(storage, "\\graphtype1_", atime, ".jpeg")
  jpeg(file1, width = 1900, height = 1200)
  plot(tab$mix.close - tab$modelclose, type = "l", ylim = 1.2*range(tab$diff))
  lines(1:nrow(tab), beta2, col = "blue")
  lines(1:nrow(tab), beta1, col = "red")
  legend("topleft", legend = c("market", "local", "global"), col = c("black", "red", "blue"), cex = 2.5, lty = c(1))
  dev.off()
  file2 <- paste0(storage, "\\graphtype2_", as.character(atime), ".jpeg")
  write.table(paste("Drawn graph type1, filename", file1), logfile, row.names = FALSE,  col.names = FALSE, append = TRUE)
  jpeg(file2, width = 1900, height = 1200)
  plot(tab$mix.time, tab$mix.close - tab$modelclose, type = "l", ylim = 1.2*range(tab$diff))
  lines(tab$mix.time, beta2, col = "blue")
  lines(tab$mix.time, beta1, col = "red")
  legend("topleft", legend = c("market", "local", "global"), col = c("black", "red", "blue"), cex = 2.5, lty = c(1))
  dev.off()
  write.table(paste("Drawn graph type2, filename", file2), logfile, row.names = FALSE,  col.names = FALSE, append = TRUE)
data.frame(Time = tab$mix.time, betalocal = beta1, betaglobal = beta2)
}

GetData <- function(Type = "Z5", src, storage = "\\\\192.168.1.204\\share\\People\\Алексей") {
  alpha <- 1.587214
  purlen <- 15
  quantlevel <- 0.05
  multip <- 1

  if (src == "FTBot") {
    tabMMVB <- LoadBp(paste0("MM", Type, "@FORTS"), as.Date("2015-09-16"), Sys.Date(), "", storage.path = storage)
    if (nrow(tabMMVB) == 0) {
      stop("No data! Try Finam instead of FTBot!")
    }  
    tabRTS <- LoadBp(paste0("RI", Type, "@FORTS"), as.Date("2015-09-16"), Sys.Date(), "", storage.path = storage)
    tabSI <- LoadBp(paste0("SI", Type, "@FORTS"), as.Date("2015-09-16"), Sys.Date(), "", storage.path = storage)

    tab <- InnerMergeDf(list(mix = tabMMVB, rts = tabRTS, si = tabSI), "Time")
    tab <- na.omit(tab)
    names(tab) <- tolower(names(tab))

    tab$Time <- tab$mix.time
  } else if (src == "Finam") {
    tabMMVB <- read.table(paste(storage, "FinamData", "SPFB.MXI.txt", sep = "\\"), sep = ",", header = TRUE)
    tabRTS <- read.table(paste(storage, "FinamData", "SPFB.RTS.txt", sep = "\\"), sep = ",", header = TRUE)
    tabSI <- read.table(paste(storage, "FinamData", "SPFB.Si.txt", sep = "\\"), sep = ",", header = TRUE)    

    tabMMVB <- Formatize(tabMMVB)
    tabRTS <- Formatize(tabRTS)
    tabSI <- Formatize(tabSI)

    tab <- InnerMergeDf(list(mix = tabMMVB, rts = tabRTS, si = tabSI), "time")
    tab$Time <- tab$mix.time
    tab$mix.bid <- tab$mix.close
    tab$mix.ask <- tab$mix.close
    tab$rts.bid <- tab$rts.close
    tab$rts.ask <- tab$rts.close
    tab$si.bid <- tab$si.close
    tab$si.ask <- tab$si.close
  } else {
    stop("Wrong choice of source! Should be either Finam or FTBot!")
  }

  mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"))
  tab <- tab[mask>0, ]
 
  print(summary(tab))
  ind1 <- Purify2(tab, len = purlen*multip, strname = "mix.")
  ind2 <- Purify2(tab, len = purlen*multip, strname = "rts.")
  ind3 <- Purify2(tab, len = purlen*multip, strname = "si.")
  tab2 <- tab[-union(ind1, union(ind2, ind3)), ]
###tab2 <- tab

  tab2$mix.close <- (tab2$mix.bid + tab2$mix.ask)/2
  tab2$mix.bid <- 100*tab2$mix.bid
  tab2$mix.ask <- 100*tab2$mix.ask
  tab2$mix.close <- 100*tab2$mix.close
  tab2$modelask <- alpha*0.00002*tab2$rts.ask*tab2$si.ask
  tab2$modelbid <- alpha*0.00002*tab2$rts.bid*tab2$si.bid
  tab2$modelclose <- (tab2$modelbid + tab2$modelask)/2
  tab2$diff <- tab2$mix.close - tab2$modelclose
###return(tab2)
  tab3 <- KillQuantiles(tab2, quantlevel, "diff", purlen*multip)

tab3  
}
GetRollingBetas <- function(tab, winlen) {
print("GetRollingBetas")
  vec0 <- tab$mix.close - tab$modelclose
  vec <-  rollmeanr(vec0, winlen)
  vec2 <- rollapplyr(vec0[1:winlen-1], winlen, mean, partial = TRUE)
c(vec2, vec)
}

####from qlib
origin <- as.Date("1970-01-01")
LoadBp <- function(instrument, start.date, end.date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp")) {
	as.data.frame(
		do.call(
			rbind, lapply(as.Date(start.date:end.date, origin=origin), function(date) {
				LoadBpDay(instrument, date, candle.type, only.main.session, start.time, end.time, storage.path)
			})
		)
	)
}
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
CreateBp <- function() {
	data.frame(
		Time=as.POSIXct(numeric(), origin=origin, tz="UTC"),
		Bid=numeric(),
		Ask=numeric(),
		Date=as.Date(numeric(), origin=origin),
		stringsAsFactors = FALSE
	)
}
LoadBpFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL) {
	df <- read.csv2(file, as.is = TRUE, na.strings="-1")
	df$Time <- as.POSIXct(df$Time, origin=origin, tz="UTC", format = "%Y-%m-%d %H:%M:%OS")
	df$Date <- as.Date(df$Time, origin=origin)
	df$BatchTime <- NULL
	df$Bid <- as.numeric(df$Bid)
	df$Ask <- as.numeric(df$Ask)
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
InnerMergeDf <- function(list.df, key){
	# key is the name of the key-value
	# return merdge df
  df.start <- list.df[[1]]
  if (is.null(df.start[, key])) {
    stop("data.frame N1 does not have the key column!")
  }
  if (length(list.df) > 1) {
    for (i in 2:length(list.df)) {
      df.next <- list.df[[i]]
      if (is.null(df.start[, key])) {
        stop(paste0("data.frame ", i, " does not have time column!"))
      }
      ind.start <- df.start[, key] %in% df.next[, key]
      ind.next <- df.next[, key] %in% df.start[, key]
      df.start <- cbind(df.start[ind.start, ], df.next[ind.next, ])
    }
  }

  i <- 1
  j <- 1
  while (j<=ncol(df.start)) {
    df.next <- list.df[[i]]
    for (k in 1:ncol(df.next)) {
      names(df.start)[j] <- paste0(names(list.df[i]), ".", names(df.next)[k])
      j <- j + 1
    }
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
    mask[which((df$Time >= as.POSIXct(paste(df$Date, start.val))) &
    					 	(df$Time <= as.POSIXct(paste(df$Date, end.val))) &
    					 	(df$Date %in% days))] <- 1
  	} else {
  	  mask[which((df$Time >= as.POSIXct(paste(df$Date, start.val), origin=origin, tz=tz)) &
  	               (df$Time <= as.POSIXct(paste(df$Date, end.val), origin=origin, tz=tz)) &
  	               (df$Date %in% days))] <- 1
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
Formatize <- function(tab) {
  times <- strptime(paste(tab$X.DATE., tab$X.TIME), format = "%Y%m%d %H:%M:%S", tz = "UTC")
  closes <- tab$X.CLOSE.
  data.frame(time = times, close = closes, date = as.Date(times))
}

