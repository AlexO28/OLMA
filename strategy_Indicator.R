# simple  strategy
# ENTRY LEVEL: indicator which  has to be %IN% {0,1,-1}
# exit level : opposit signal and mask = 2
# levels means buy and sell signals
# strategy params
# there is no params
# maxposa = 1
#Bid Ask data only
StrategyIndicator<- function(df, mask, indicator, slippage) {

  trades <- create.trades()
  curpos <- 0

  for(r in 1:nrow(df)) {
    candle = df[r, ]
    if(mask[r] == 1) {
      # main sycle
      if(indicator[r] == -1 && curpos > -1) {
        curpos <- curpos - 1
        sell <- Sell(candle, slippage=slippage, comment="sell")
        trades <- rbind(trades, sell)
      } else if (indicator[r] == 1 && curpos < 1) {
        curpos <- curpos + 1
        buy <- Buy(candle, slippage=slippage, comment="buy")
        trades <- rbind(trades, buy)
      }
    } else if(mask[r] == 2 && curpos != 0) {
      # close positions
      trades <- rbind(trades, ClosePsition(candle, curpos, slippage=slippage))
      curpos <- 0
    }
  }
  trades
}
