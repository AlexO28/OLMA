# TODO move to qlib, add description?
prepare.ticks <- function(ticks, period) {
  options(stringsAsFactors=FALSE)
	times <- c()
	closes <- c()
	volbids <- c()
	volasks <- c()
	numdealsbid <- c()
	numdealsask <- c()

  startstr <- "10:00:00"
  endstr <- "18:45:00"

  dates <- unique(as.Date(ticks$time, origin = "1970-01-01"))
  for (date in dates) {
    ticksday <- ticks[as.Date(ticks$time, origin = "1970-01-01") == date, ]
    date <- as.Date(date, origin = origin)
	  start <- as.POSIXct(paste(date, startstr), origin = origin, tz = "UTC")
    fullend  <- as.POSIXct(paste(date, endstr), origin = origin, tz = "UTC")
  	end <- as.POSIXct(start, origin = origin, tz = "UTC") + period
	  while (as.POSIXct(end, origin = origin, tz = "UTC") <= as.POSIXct(fullend, origin = origin, tz = "UTC")) {
  		ticksred <- ticksday[((ticksday$time >= start) & (ticksday$time < end)), ]
	  	times <- c(times, as.POSIXct(start))
	  	if (nrow(ticksred) > 0) {
		  	closes <- c(closes, as.numeric(as.character(ticksred$price[nrow(ticksred)])))
		  	sellticks <- ticksred[ticksred$dir==-1, ]
	  		buyticks <- ticksred[ticksred$dir==1, ]
		  	volbids <- c(volbids, ifelse(is.na(sum(sellticks$volume)), 0, sum(sellticks$volume)))
		  	volasks <- c(volasks, ifelse(is.na(sum(buyticks$volume)), 0, sum(buyticks$volume)))
		  	numdealsbid <- c(numdealsbid, nrow(sellticks))
		  	numdealsask <- c(numdealsask, nrow(buyticks))
	  	} else {
		  	ticksred <- ticksday[ticksday$time < start, ]
		  	if (nrow(ticksred) > 0) {
		  		closes <- c(closes, as.numeric(as.character(ticksred$price[nrow(ticksred)])))
		  	} else {
		  	  ticksred <- ticksday[((ticksday$time > start)), ]
		  	  closes <- c(closes, as.numeric(as.character(ticksred$price[1])))
		  	}
	  		volbids <- c(volbids, 0)
	  		volasks <- c(volasks, 0)
	  		numdealsbid <- c(numdealsbid, 0)
	  		numdealsask <- c(numdealsask, 0)
	 	  }
		  start <- end
	  	end <- as.POSIXct(start, origin = origin) + period
	}
  }
	data.frame(Time = as.POSIXct(times, origin = origin, tz = "UTC"), Close = as.numeric(as.character(closes)), Volbid = volbids, Volask = volasks,
						 NumDealsBid = numdealsbid, NumDealsAsk = numdealsask, Period = rep(period, length(times)))
}


