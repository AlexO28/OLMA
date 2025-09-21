# this shit is similar for carcas
StrategyTemplate <- function(data, mask, fair.price, koeff, maxposa, bp, sp, bdp, sdp, ...) {
  #initialize
  trades <- create.trades()
  buy.order <- NULL
  sell.order <- NULL
  position <- rep(NA, nrow(data))
  curpos <- 0
  # main cycle
  for(r in 1:nrow(df)) {
    candle = data[r, ]

    #check for execution
    #check for buy
    buy.trade <- ExecuteLimitOrder(buy.order, candle, slippage)
    sell.trade <- ExecuteLimitOrder(sell.order, candle, slippage)
    if(!is.null(buy.trade)) {
      trades <- rbind(trades, buy.trade)
      curpos <- curpos + 1
    }

    if(!is.null(sell.trade)) {
      trades <- rbind(trades, sell.trade)
      curpos <- curpos + 1
    }

    position[r] <- curpos
    # cancel orders if mask = 0
    if(mask[r] == 0) {
      buy.order <- NULL
      sell.order <- NULL
      next
    }

    # main cycle
    if(mask[r] == 1) {
      b.shift  <- bp(curpos, ...)
      s.shift <- sp(curpos, ...)
      b.speedshift <- bdp(position, r, ...)
      s.speedshift <- sdp(position, r, ...)

      buy.order <- BuyOrder(fair.price[r] + b.shift * koeff - b.speedshift)
      sell.order <- SellOrder(fair.price[r] + s.shift * koeff + s.speedshift)
      next
    }


    #close position
    if(mask[r] == 2 && curpos != 0) {

      trades <- rbind(trades, ClosePsition(candle, curpos, slippage=slippage))
      tp <- NULL
      curpos <- 0
      position[r] <- curpos
    }

  }
  trades
}
