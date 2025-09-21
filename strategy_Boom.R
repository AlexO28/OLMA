BoomStrategy <- function(df, indlow, indhigh, mask, params, maxpos, mode) {

  options(stringsAsFactors=FALSE)
  trades <- create.trades()
  eps <- 0.00001
  if (is.null(df$Bid)) {df$Bid <- df$Close}
  if (is.null(df$Ask)) {df$Ask <- df$Close}

  curpos <- 0
  deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())

  for (r in 1:nrow(df)) {
    if ((!is.na(indlow[r])) & (!is.na(indhigh[r])) & (mask[r]>0)) {
      if (mask[r] < 2) {
        candle <- df[r, ]

       if ((candle$Ask > indhigh[r]) & (curpos < maxpos)) {
      ###  if ((candle$Bid < indlow[r]) & (curpos < maxpos)) {
          if (-1 %in% deals$dir) {
            num <- length(deals$dir[deals$dir == -1])
            if (num != length(deals$dir)) {print("error in comparison! -1")}
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
          } else {
            num <- 1
            deals <- rbind(deals, data.frame(price = candle$Ask, dir = 1, volume = num, res = FALSE))
          }
          trade <- buy(candle = candle, comment = "enter", volume = num)
          trades <- rbind(trades, trade)
          curpos <- curpos + num

          #print("bought")
          #print(deals)
          #print(curpos)

        } else if ((candle$Bid < indlow[r]) & (curpos > -maxpos)) {
      #### } else if ((candle$Ask > indhigh[r]) & (curpos > -maxpos)) {
          if (1 %in% deals$dir) {
            num <- length(deals$dir[deals$dir == 1])
            if (num != length(deals$dir)) {print("error in comparison! 1")}
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
          } else {
            num <- 1
            deals <- rbind(deals, data.frame(price = candle$Bid, dir = -1, volume = num, res = FALSE))
          }
          trade <- sell(candle = candle, comment = "enter", volume = num)
          trades <- rbind(trades, trade)
          curpos <- curpos - num

          #print("sold")
          #print(deals)
          #print(curpos)
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
