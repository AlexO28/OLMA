# help functions-------------------------------
GetVolume <- function(trades) {
  sum(trades$Volume)
}
GetPosition  <- function(trades) {
  pos = ifelse(nrow(trades)==0, 0, sum(trades$Volume * trades$Dir))
  pos
}
IsClosedPos <- function(trades) {
  !sum(trades$Volume * trades$Dir)
}
GetMaxOpenedPos <- function(trades) {
  max(abs(cumsum(trades$Volume * trades$Dir)))
}
get.backtest.interval <- function(df) {
	length <- nrow(df)
	new_interval(as.character(df$Time[1]), as.character(df$Time[length]))
}
stat.curve <- function(df, trades) {
	open.pos <- rep(0, nrow(df))
	money <- rep(0, nrow(df))
	for(i in 1:nrow(trades)) {
		shit <- which(df$Time == trades[i, "time"])
		r <- trades[i, "dir"] * trades[i, "volume"]
		open.pos[shit] <- open.pos[shit] + r
		money[shit] <- money[shit] - r * trades[i, "price"]
	}
	open.pos <- cumsum(open.pos)
	money <- cumsum(money)
	equity <- money + open.pos * df$Close
	drawdown <- cummax(equity) - equity
	list(
		position=open.pos,
		equity=equity,
		drawdown=drawdown)
}

# scoring functions for optim------------------
get.pl <- function(trades, per.trade=FALSE, commis=0, point.price=1) {
   if(nrow(trades)==0)
     return(0)
   pl <- with(trades, point.price * sum(volume*price*(-dir)) - commis * sum(volume))
   if(per.trade) {
   	pl <- pl / nrow(trades)
   }
   pl
}

get.pl.per.day <- function(trades, commis=0, point.price=1) {
    vec <-as.Date(as.POSIXct(trades$time, origin = origin, tz = "UTC"), origin=origin)
  tab <- split(trades, vec)
  data.frame(date = unique(vec), pl = sapply(tab, get.pl, commis=commis, point.price=point.price))
}

get.pl.per.day.instr <- function(trades, info) {
  vec <-as.Date(as.POSIXct(trades$time, origin = origin, tz = "UTC"), origin=origin)
  tab <- split(trades, vec)
  do.call(rbind, lapply(tab, get.pl.instr, info = info))
}

get.pl.instr <- function(trades, info, mode=2) {
  res <- data.frame(instr = info$instrument, pl = 0)

  if (nrow(trades) > 0) {
    tab <- merge(trades, info, by.x = "instrument", by.y = "instrument")

    fun <- function(tab) {
      aaa <- sum(tab$point.price * tab$volume * tab$price * (-tab$dir)) - sum(tab$commis * tab$volume)
      ###print(aaa)
      aaa
    }

    temptab <- split(tab, tab$instrument)
  if (mode == 1) {
    res <- data.frame(date = as.Date(trades$time[1]), instr = unique(tab$instr), pl = sapply(temptab, fun))
  } else {
    res <- data.frame(date = as.Date(trades$time[1]), pl=sum(tab$point.price * tab$volume * tab$price * (-tab$dir)) - sum(tab$commis * tab$volume))
  }
  }
  res
}

#TO DO fatality
get.stat <- function(trades, df, info = NULL, start.money = 0, candle.type=NA, instrument=NA, args=NA, name = NA) {
	stat <- stat.curve(df, trades)
	#if(is.null(info)) info <- default.info()

	# common stats
	int <- get.backtest.interval(df) / edays(1)
	volume <- get.volume(trades)
	n.of.trades <- nrow(trades)
	total.commision <- info$commis * volume
  max.opened.posa <- get.max.opened.pos(trades)
  n.of.sl <- sum(grepl(trades$comment,pattern="sl"))
	n.of.tp <- sum(grepl(trades$comment,pattern="tp"))
	daypl <- get.pl.per.day(trades, info$commis, info$point.price)

	# cummulative stats
	total.pl.rub <- ifelse(is.null(info$point.price), NA, get.pl(trades, commis=info$commis, point.price=info$point.price)) #profit in rub
	total.pl.points <- get.pl(trades, commis = ifelse(is.null(info), 0, info$commis))

	max.drawdown.points <- max(stat$drawdown)
	max.drawdown.rub <- ifelse(is.null(info$point.price), NA, max.drawdown.points * info$point.price)

	pl.return = total.pl.rub / (start.money)
	year.return = pl.return/int*365

	# curve stats
	equity.rub <- stat$equity * info$point.price
	equity.points <- stat$equity
	equity.rel <- equity.rub / start.money  # okey thats what we need

	drawdown.rub <-  -stat$drawdown * info$point.price
	drawdown.rel <- stat$drawdown / (equity.points + start.money)

  list(
  	# common stats
    test.interval = new_interval(df$Time[1], df$Time[nrow(df)]),
    start.money = start.money,
    maxpos = max.opened.posa,
  	int = int,
  	volume = volume,
  	n.of.trades = n.of.trades,
    n.per.day = n.of.trades/int,
  	total.commision = total.commision,

  	# cummulative stats
  	total.pl.rub = total.pl.rub,
  	total.pl.points = total.pl.points,
  	year.return = year.return,
		pl.return = pl.return,
  	max.drawdown.points = max.drawdown.points,
  	max.drawdown.rub = max.drawdown.rub,
    max.drawdown.rel = max(drawdown.rel),
  	# curve stats
		equity.rub = equity.rub,
		equity.points = equity.points,
		equity.rel = equity.rel,

		drawdown.rub = drawdown.rub,
		drawdown.rel = drawdown.rel,

  	# smth else
    dates = unique(as.Date(df$Time)),
    df = df,
    trades = trades,

    tp = n.of.tp,
    sl = n.of.sl,

		instrument = instrument,
		candle.type = candle.type,
    args = args,
		name = name
  )
}




create.result <- function() {
	options(stringsAsFactors = FALSE)
	data.frame(
		name = character(),
		ctype = character(),
		ins = character(),
		int = numeric(),
		args = character(),
		pl = numeric(),
		dd = numeric(),
		year = numeric(),
		pl.rub = numeric(),
		dd.rub = numeric(),
		start = numeric(),
		maxopened = numeric(),
		n.per.day = numeric(),
		commis = numeric()
	)
}

add.result <- function(stat) {
	options(stringsAsFactors = FALSE)
	list(
		name = stat$name,
		ctype = stat$candle.type,
		ins = stat$instrument,
		int = stat$int,
		args = do.call(paste, args=c(as.list(stat$args), sep=",")),
		pl = stat$pl.return,
		dd = stat$max.drawdown.rel,
		year = stat$year.return,
		pl.rub = stat$total.pl.rub,
		dd.rub = stat$max.drawdown.rub,
		start = stat$start.money,
		maxopened = stat$maxpos,
		n.per.day = stat$n.per.day,
		commis = stat$total.commision)

}
