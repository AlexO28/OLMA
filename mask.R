#creates mask for datframe df from times:
#start[1]....end[1], ..., start[length(start)]...end{length(start)}
#days --- list of days for which we get 1
# use start as hh:mm:ss (character)
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


# start,end might be as.Date, POSIXct etc
# this function return logical mask: which rows belongs to interval
MaskInterval <- function(df, start=df$Time[1], end=df$Time[nrow(df)]) {
	if(is.character(start)) start <- as.POSIXct(start, origin=origin, tz="UTC")
	if(is.character(end)) end <- as.POSIXct(end, origin=origin, tz="UTC")
	interval = new_interval(start, end)
	df$Time %within% interval
}
