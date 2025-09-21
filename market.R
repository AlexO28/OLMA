#-------MARKET ORDERS--------------------------------------------------------
# use buy sell closeposition functions as usual.
# FAQ for working with tp- sl- functions:
# Order comsisits of two levels - Sl and Tp
# you may create Order with PlaceOrder function.
# then use ExecuteOrder which return trade if one of two orders(sl or tp) executed
# if strategy set several stoporders or tp, use data.frame of orders (CreateOrder() for first step, then add with PlaceOrder)
# At this way use CheckStopOrders for checking execution instead of ExecuteOrder. this function return df of trades
# and df of stop.orders (refreshed)

# I've dropped alexey's checking-function because they do exactly the same but slower


# create Empty order row. each order consist of TP and SL levels.
CreateOrder <- function() {
	data.frame(
		Time=as.POSIXct(numeric(), origin=origin, tz="UTC"),
		Dir=numeric(),
		Tp=numeric(),
		Sl=numeric(),
		Volume=numeric(),
		Comment=character(),
		Date=as.Date(numeric(), origin=origin),
		stringsAsFactors = FALSE
	)
}

# main market functions. get BA or Candle or Trade - return trade. use limit.price if limit.order
Buy <- function(candle, volume=1, comment="", slippage=0, limit.price=NULL, instrument=NA) {
	market.price = ifelse(!is.null(candle$Close), candle$Close, candle$Ask)
  b <- list(
    Time = candle$Time,
    Dir = 1,
    Price = ifelse(is.null(limit.price),market.price + slippage, limit.price),
    Volume = volume,
    Comment = ifelse(comment == "", "buy", paste0("buy [", comment, "]")),
 		Instrument = instrument,
    Date = candle$Date,
    stringsAsFactors = FALSE
  )
  b
}

# same story for sell
Sell <- function(candle, volume=1, comment="", slippage=0, limit.price=NULL, instrument=NA) {
		market.price = ifelse(!is.null(candle$Close), candle$Close, candle$Bid)
	 	s <- list(
    Time = candle$Time,
    Dir = -1,
    Price = ifelse(is.null(limit.price), market.price - slippage, limit.price),
    Volume = volume,
    Comment = ifelse(comment=="", "sell", paste0("sell [", comment, "]")),
    Instrument = instrument,
    Date = candle$Date,
    stringsAsFactors = FALSE
  )
  s
}

# set one stoploss and takeprofit after trade
PlaceOrder <- function(trade, takeprofit, stoploss){
	list(
		Time = trade$Time,
		Dir = -trade$Dir,
		Tp = trade$Price + trade$Dir * takeprofit,
		Sl = trade$Price - trade$Dir * stoploss,
		Volume = Order$volume,
		Comment = "",
		Date = NA
	)
}

# get order and check its execution on current candle or current best price pair
ExecuteOrder <- function(order, candle, slippage, limit.tp=T) {
	if(is.NULL(order))return(NULL)

	up.price = ifelse(!is.null(candle$High), candle$High, candle$Bid)
	low.price = ifelse(!is.null(candle$Low), candle$Low, candle$Ask)

	if(order$Dir == 1 && order$Sl <= up.price) {
		Buy(candle, comment=paste0("sl-", order$Time), limit.price=order$Sl, slippage=slippage)
	} else if(order$Dir == -1 && order$Sl >= low.price) {
		Sell(candle, comment=paste0("sl-", order$Time), limit.price=order$Sl, slippage=slippage)
	} else if(order$Dir == 1 && order$Tp > low.price) {
		Buy(candle, comment=paste0("tp-", order$Time),
				limit.price=ifelse(limit.tp, order$Tp, ifelse(!is.null(candle$Close), candle$Close, candle$Ask) + slippage))
	} else if(order$Dir == -1 && order$Tp < up.price) {
		Sell(candle, comment=paste0("tp-", order$Time),
				 limit.price=ifelse(limit.tp, order$Tp, ifelse(!is.null(candle$Close), candle$Close, candle$Ask) - slippage))
	}
}


# close position in the end of work
ClosePosition <- function(candle, position, slippage, instrument=NA){

  if (position > 0) {
    Sell(candle, volume=position, comment="close", slippage=slippage, instrument=instrument)
  } else if (position < 0) {
    Buy(candle, volume=abs(position), comment="close", slippage=slippage, instrument=instrument)
  } else {
    CreateTrade()
  }
}


# this is ExecuteOrder for df of orders. return list(executed.trades and new df of stop.orders). use if more than one stop.order
CheckStopOrders <- function(candle, stop.orders, slippage, limit.tp=T) {
  trades <- CreateTrade()
  executed <- rep(F, nrow(stop.orders))
  for(c in 1:nrow(stop.orders)) {
  		trade <- ExecuteOrder(stop.orders[c, ], candle, slippage, limit.tp)
  		if(!is.null(trade)) {
  			trades <- rbind(trades, trade)
  			executed[c] <- TRUE
  		}
  }
	stop.orders <- stop.orders[!executed, ]
	list(trades=trades, stop.orders=stop.orders)
}
