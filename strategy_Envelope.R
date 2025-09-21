# stochastic strategy -> enter when residualsfrom indicator more than param
# strategy params
# 1. treshold level of deviation from indicator,


StrategyEnvelope <- function(df, mask, indicator, slippage, param) {
	options(stringsAsFactors = FALSE)

	#create tp-order,curpos,trades
	trades <- CreateOrder()
	curpos <- 0

	#create signals
	buy.signal <- df$Ask <= indicator - param
	sell.signal <- df$Bid >= indicator + param


	for (r in 1:nrow(df)) {
		candle = df[r, ]
		# main cycle -------------------------------
		if (mask[r] == 1) {
			if (sell.signal[r] && curpos > -1) {
				# SELL signal
				curpos <- curpos - 1
				sell <- Sell(candle, slippage = slippage, comment = "sell", limit.price = indicator[r] + param)
				trades <- rbind(trades, sell)

			} else if (buy.signal[r] && curpos < 1) {
				curpos <- curpos + 1
				buy <- Buy(candle, slippage = slippage, comment = "buy", limit.price = indicator[r] - param)
				trades <- rbind(trades, buy)
			}
		}
		else if (mask[r] == 2) {
		  if (curpos != 0) {
		    trades <- rbind(trades, ClosePosition(candle, curpos, slippage = slippage))
		  }
		  curpos = 0
		}
	}
	trades
}



