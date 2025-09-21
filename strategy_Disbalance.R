##список параметров: low, high --- нижнее и верхнее значения индикатора, stoploss, takeprofit
DisbalanceStrategy <- function(df, ind, maxpos, mask, params, mode) {
  options(stringsAsFactors=FALSE)
  trades <- create.trades()
  eps <- 0.00001
  if (is.null(df$Bid)) {df$Bid <- df$Close}
  if (is.null(df$Ask)) {df$Ask <- df$Close}

  curpos <- 0
  deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())

  for (r in 1:nrow(df)) {
    if ((!is.na(ind[r])) & (mask[r]>0)) {
      if (mask[r] < 2) {
        candle <- df[r, ]
        if (ind[r] < params$low & curpos < maxpos) {
          if (curpos<0) {
            num <- abs(curpos)
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
          } else {
            num <- 1
            deals <- rbind(deals, data.frame(price = candle$Ask, dir = 1, volume = num, res = FALSE))
          }
          trade <- buy(candle = candle, volume = num, comment = "enter")
          trades <- rbind(trades, trade)
          curpos <- curpos + num
        } else if (ind[r] > params$high & curpos > -maxpos) {
          if (curpos>0) {
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
            num <- curpos
          } else {
            num <- 1
            deals <- rbind(deals, data.frame(price = candle$Bid, dir = -1, volume = num, res = FALSE))
          }
          trade <- sell(candle = candle, volume = num, comment = "enter")
          trades <- rbind(trades, trade)
          curpos <- curpos - num
        }

        if (nrow(deals)>0) {
          checkeddeals <- FullStopLossCheck(deals, candle, params$stoploss)
          temp <- MakeTradesCheck(checkeddeals, "stoplossed", curpos, candle)
          deals <- temp$deals
          curpos <- temp$curpos
          trades <- rbind(trades, rbind(c(), temp$trades))
          if (nrow(deals) > 0) {
            checkeddeals <- FullTakeProfitCheck(deals, candle, params$takeprofit)
            temp <- MakeTradesCheck(checkeddeals, "takeprofitted", curpos, candle, mode = mode)
            deals <- temp$deals
            curpos <- temp$curpos
            trades <- rbind(trades, rbind(c(), temp$trades))
            ###if (nrow(deals)==0) {print("takeprofitted")}
          } ####else {print("stoplossed")}
        }
      } else {
        if ((mask[r] == 2) & (curpos != 0)) {
          trades <- rbind(trades, close.position(candle, curpos, slippage=0))
          curpos <- 0
        }
      }
    }
  }
trades$time <- as.POSIXct(trades$time, origin = "1970-01-01")
trades
}
