# stochastic strategy -> enter when maxp
# strategy params
# 1. treshold level of stochastik,
# 2. net step size
# 3. tp
# 4. treshold level exit

StrategyStochasticNet <- function(df, mask, indicator, maxpos, slippage, params) {
  entry.level = params[1]
  shift = params[2]
  takeprofit = params[3]
  exit.level = params[4]
  options(stringsAsFactors=FALSE)

  #create tp-order,curpos,trades
  trades <- create.trades()
  tp  <- NULL #null means tp is not exists yet
  curpos <- 0

  #create signals
  sell.signal <- 100 - entry.level
  buy.sugnal <- entry.level


  for(r in 1:nrow(df)) {
    candle = df[r, ]

    # check for takeprofit execution ------------
    if(!is.null(tp)) {
    	trade <- ExecuteOrder(tp, candle, slippage)
    	if(!is.null(trade)) {
    	trades <- rbind(trades, trade)
    	curpos <- curpos + trade$Dir
    	}
    }

    # if 0 set to start.position ---------------
    if(curpos == 0) {
      sell.signal <- 100 - entry.level
      buy.signal <- entry.level
    }

    # main cycle -------------------------------
    if(mask[r] == 1) {
      if(indicator[r] >= sell.signal && curpos > -maxpos) {
      	# SELL signal
        curpos <- curpos - 1
        sell <- Sell(candle, slippage=slippage, comment=paste("sell, stoch >=", sell.signal))
        trades <- rbind(trades, sell)

        # set for next signal levels
        sell.signal <- sell.signal + shift
        buy.signal <- buy.signal + shift
      } else if (indicator[r] <= buy.signal && curpos < maxpos) {
        curpos <- curpos + 1
        buy <- Buy(candle, slippage=slippage, comment=paste("buy, stoch <=", buy.signal))
        trades <- rbind(trades, buy)

        # set for next signal levels
        sell.signal <- sell.signal - shift
        buy.signal <- buy.signal - shift
      }

      if(curpos == -1 && is.null(tp)) {
      	# set new exit rule:
        buy.signal <- exit.level
        tp <- BuyOrder(trades$Price[nrow(trades)], 1, "takeprofit", candle$Time)
      }
      if(curpos == 1 && is.null(tp)) {
        sell.signal = 100 - exit.level
        tp <- SellOrder(trades$Price[nrow(trades)], 1, "takeprofit", candle$Time)
      }
    } else if(mask[r] == 2 && curpos != 0) {
      trades <- rbind(trades, ClosePsition(candle, curpos, slippage=slippage))
      tp <- NULL
      curpos <- 0
    }
  }
  trades
}
