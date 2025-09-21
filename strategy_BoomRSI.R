BoomStrategyRSI <- function(df, indlow, indhigh, exitind, mask, params, maxpos) {
  options(stringsAsFactors=FALSE)
  trades <- create.trades()
  eps <- 0.00001
  if (is.null(df$Bid)) {df$Bid <- df$Close}
  if (is.null(df$Ask)) {df$Ask <- df$Close}

  curpos <- 0
  deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())

  flagbuy <- FALSE
  flagsell <- FALSE

  for (r in 1:nrow(df)) {
    if ((!is.na(indlow[r])) & (!is.na(indhigh[r])) & (!is.na(exitind[r])) & (mask[r]>0)) {
      if (mask[r] < 2) {
        candle <- df[r, ]
        if ((candle$Ask > indhigh[r]) & (curpos < maxpos)) {
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
        }

        if (nrow(deals)>0) {
          checkeddeals <- FullStopLossCheck(deals, candle, params$stoploss)
          temp <- MakeTradesCheck(checkeddeals, "stoplossed", curpos, candle, mode = "normal")
          deals <- temp$deals
          curpos <- temp$curpos
          trades <- rbind(trades, rbind(c(), temp$trades))
        }

        if (curpos > 0) {
          exitinfo <- WonderEnterAlt(flagsell, exitind[r], 100-params$threshold)
          if (exitinfo$res > 0) {
            trade <- sell(candle, comment="exit", volume = abs(curpos))
            curpos <- 0
            trades <- rbind(trades, trade)
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
          }
          flagsell <- exitinfo$flag
        } else if (curpos < 0) {
          exitinfo <- WonderEnter(flagbuy, exitind[r], params$threshold)
          if (exitinfo$res > 0) {
            trade <- buy(candle, comment="exit", volume = abs(curpos))
            curpos <- 0
            trades <- rbind(trades, trade)
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
          }
          flagbuy <- exitinfo$flag
        }
      } else {
        if ((mask[r] == 2) & (curpos != 0)) {
          flagbuy <- FALSE
          flagsell <- FALSE
          trades <- rbind(trades, close.position(candle, curpos, slippage = 0))
          curpos <- 0
        }
      }
    }
  }
trades$time <- as.POSIXct(trades$time, origin = "1970-01-01")
trades
}
