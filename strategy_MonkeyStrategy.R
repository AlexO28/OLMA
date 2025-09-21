# description:
#
# short entry rule:  indicator = -1
# long entry rule:  indicator = 1
# exit rule (for each deal separately) - standart takeprofit, standart stoploss
# maxposa - maximal position value
# mask = trade.mask {0,1,2}

myMonkeyStrategy <- function(df, mask, indicator, maxpos, slippage, takeprofit, stoploss) {
  options(stringsAsFactors=FALSE)
	trades <- create.trades()
	stop.orders <- create.orders()
  curpos <- 0
	for(r in 1:nrow(df)) {
    candle = df[r, ]
    # cexecute stop.orders
    check <- check.stop.orders(candle, stop.orders=stop.orders, slippage=slippage)
    trades <- rbind(trades, check$trades)
    stop.orders <- check$stop.orders
    curpos <- get.position(trades)

	  # main cycle
	  if(mask[r] == 1) {
	    if(indicator[r] == -1 && curpos > -maxpos) {
	      curpos <- curpos - 1
	      sell <- sell(candle, slippage=slippage)
	      stop.order <- place.order(sell, takeprofit, stoploss)
	      stop.orders <- rbind(stop.orders, stop.order)
	      trades <- rbind(trades, sell)
	    } else if (indicator[r] == 1 && curpos < maxpos) {
	      curpos <- curpos + 1
	      buy <- buy(candle, slippage=slippage)
	      stop.order <- place.order(buy, takeprofit, stoploss)
	      stop.orders <- rbind(stop.orders, stop.order)
	      trades <- rbind(trades, buy)
	    }
	  } else if(mask[r] == 2 && curpos != 0) {
	    trades <- rbind(trades, close.position(candle, curpos, slippage=slippage))
	    stop.orders <- create.orders()
	    curpos <- 0
	  }
	}
	trades
}


