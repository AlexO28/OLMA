###params is a data-frame that contains
###threshold, n, stoploss, K
WonderStrategy <- function(df, enterindicator, exitindicator, mask, maxpos, slippage, params, control = 1) {
  options(stringsAsFactors=FALSE)
  trades <- create.trades()
  eps <- 0.00001

  if (is.null(df$Bid)) {df$Bid <- df$Close}
  if (is.null(df$Ask)) {df$Ask <- df$Close}
  if (is.null(df$Low)) {df$Low <- df$Bid}
  if (is.null(df$High)) {df$High <- df$Ask}

  flag <- 0
  foundtrigger <- 0
  curpos <- 0

  skip <- params$n

  deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())

  for(r in (1):nrow(df)) {
    if (!is.na(enterindicator[r]) & !is.na(exitindicator[r]) & (mask[r]>0)) {
      if (mask[r] < 2) {
        candle <- df[r, ]
        if (control == 1) {
          enterinfo <- WonderEnter(flag, enterindicator[r], params$threshold)
          if ((enterinfo$res > 0) & (curpos < maxpos) & (r>skip)) {
            #  print(c(df$Time[r-1]))
            #  print(c(df$Close[r], enterindicator[r-1], enterindicator[r]))
            #  print("***")
            trade <- Buy(candle, slippage=slippage, comment = "enter")
            deals <- as.data.frame(rbind(c(), deals, c(candle$Ask, 1, 1, FALSE)))
            names(deals) <- c("price", "dir", "volume", "res")
            curpos <- curpos + 1
            trades <- rbind(trades, trade)
          }
        } else {
          enterinfo <- WonderEnterAlt(flag, enterindicator[r], 100-params$threshold)
          #print("***")
          #print(enterinfo)
          if ((enterinfo$res > 0) & (curpos > -maxpos) & (r>skip)) {
            trade <- Sell(candle, slippage=slippage, comment="enter")
            deals <- as.data.frame(rbind(deals, c(candle$Bid, -1, 1, FALSE)))
            names(deals) <- c("price", "dir", "volume", "res")
            curpos <- curpos - 1
            trades <- rbind(trades, trade)
          }
        }
        flag <- enterinfo$flag
        if (nrow(deals)>0) {
          checkeddeals <- FullStopLossCheck(deals, candle, params$stoploss)
          deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
          for (k in 1:nrow(checkeddeals)) {
            if (checkeddeals$res[k] == FALSE) {
              deals <- rbind(c(), deals, checkeddeals[k,])
            } else {
              if (checkeddeals$dir[k] > 0) {
                trade <- Sell(candle, slippage=slippage, volume = checkeddeals$volume[k],  comment = "stoplossed")
                curpos <- curpos - 1
                trades <- rbind(trades, trade)
              } else {
                trade <- Buy(candle, slippage=slippage, volume = checkeddeals$volume[k], comment = "stoplossed")
                curpos <- curpos + 1
                trades <- rbind(trades, trade)
              }
            }
          }
        }

        if (control == 1) {
          exitinfo <- WonderExit(candle, exitindicator[(max(1, r-1)):(r)], foundtrigger, params$K)
          ###if ((exitinfo$res > 0) & (curpos > 0) & (exitinfo$res<=candle$Close)) {
          if ((exitinfo$res > 0) & (curpos > 0)) {
            trade <- Sell(candle, volume = abs(curpos), slippage = slippage, limit.price = exitinfo$res, comment = "exit")
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
            # print(c(df$Low[r-2], df$Low[r-1], df$Close[r]))
            #  print(df$Time[r])
            #  print(c(exitindicator[r-2], exitindicator[r-1], exitindicator[r]))
            #  print(exitinfo$res)
            #  print("&&&")
            curpos <- 0
            trades <- rbind(trades, trade)
          }
        } else {
          exitinfo <- WonderExitAlt(candle, exitindicator[(max(1, r-1)):(r)], foundtrigger, params$K)
          #print("&&&")
          #print(exitinfo)
          if ((exitinfo$res > 0) & (curpos < 0)) {
            trade <- Buy(candle, volume = abs(curpos), slippage = slippage, limit.price = exitinfo$res, comment = "exit")
            deals <- data.frame(price = numeric(), dir = numeric(), volume = numeric(), res = numeric())
            curpos <- 0
            trades <- rbind(trades, trade)
          }
        }
        foundtrigger <- exitinfo$foundtrigger
      } else {
        if ((mask[r] == 2) & (curpos != 0)) {
          trades <- rbind(trades, close.position(candle, curpos, slippage=slippage))
          curpos <- 0
        }
      }
    }
  }
  trades
}


WonderEnter <- function(flag, ind, threshold) {
  res <- 0
  if (ind > threshold) {
    if (flag == 1) {
      res <- 1
      flag <- 0
    }
  }
  if (ind <= threshold) {
    flag <- 1
  }
  list(res=res, flag=flag)
}
WonderEnterAlt <- function(flag, ind, threshold) {
  res <- 0
  if (ind < threshold) {
    if (flag == 1) {
      res <- 1
      flag <- 0
    }
  }
  if (ind >= threshold) {
    flag <- 1
  }
  list(res = res, flag = flag)
}
###foundtrigger
WonderExit <- function(candle, inds, foundtrigger, K) {
  res <- 0
  if (foundtrigger >= K) {
    if (inds[length(inds)] >= candle$Low) {
      foundtrigger <- 0
      res <- inds[length(inds) - 1]
    } else {
      foundtrigger <- foundtrigger + 1
    }
  } else {
    if (inds[length(inds)] < candle$Low) {
      foundtrigger <- foundtrigger + 1
    } else {
      foundtrigger <- 0
    }
  }
  list(foundtrigger = foundtrigger, res = res)
}
WonderExitAlt <- function(candle, inds, foundtrigger, K) {
  res <- 0
  if (foundtrigger >= K) {
    if (inds[length(inds)] <= candle$High) {
      foundtrigger <- 0
      res <- inds[length(inds) - 1]
    } else {
      foundtrigger <- foundtrigger + 1
    }
  } else {
    if (inds[length(inds)] > candle$High) {
      foundtrigger <- foundtrigger + 1
    } else {
      foundtrigger <- 0
    }
  }
  list(foundtrigger = foundtrigger, res = res)
}
