myNetStrategy <- function(df, indicator, maxpos, slippage=0, params, internal = TRUE, stepprice, stepval, commis, GO) {
  names(df) <- capitalize(names(df))
  setkey(df, "Date")
  curpos <- 0
  eps <- 0.00001
  if (is.null(df$Bid)) {df$Bid <- df$Close}
  if (is.null(df$Ask)) {df$Ask <- df$Close}

  dates <- unique(df$Date)
  restab <- data.frame(Date = dates, pl = numeric(length(dates)),
                       numdeals = numeric(length(dates)), maxpos = numeric(length(dates)))
  lastday <- FALSE

  curpos <- 0
  plloc <- 0
  numdealsloc <- 0
  maxposeff <- 0

  for (i in 1:length(dates)) {
    if (i == length(dates)) {lastday == TRUE}
    if (internal) {
      curpos <- 0
      plloc <- 0
      numdealsloc <- 0
      maxposeff <- 0
    }

    adate <- dates[i]
    dfred <- df[Date == adate, ]
    indicatorred <- indicator[Date == adate, ]

    for(r in 1:nrow(dfred)) {
      modshift <- -curpos * params$shift
      if (!is.na(indicatorred$Val[r])) {
        if (dfred$Bid[r] > indicatorred$Val[r] + params$size + modshift - eps) {
          if (curpos > -maxpos) {
              #trade <- sell(candle, slippage=slippage, limit.price = indicator[r] + params$size[r] + modshift - eps)
              plloc <- plloc + (indicatorred$Val[r] + params$size + modshift - eps - slippage)
              curpos <- curpos - 1
              numdealsloc <- numdealsloc + 1
          }
        } else if (dfred$Ask[r] < indicatorred$Val[r] - params$size + modshift + eps) {
          if (curpos < maxpos) {
              #trade <- buy(candle, slippage=slippage, limit.price = indicator[r] - params$size[r] + modshift + eps)
              plloc <- plloc - (indicatorred$Val[r] - params$size + modshift + eps - slippage)
              curpos <- curpos + 1
              numdealsloc <- numdealsloc + 1
          }
        }
      }
      if (maxposeff < abs(curpos)) {maxposeff <- abs(curpos)}
    }
    r <- nrow(dfred)
    if (internal | lastday) {
      if (curpos > 0) {
          plloc <- plloc + dfred$Bid[r]*abs(curpos)
          numdealsloc <- numdealsloc + abs(curpos)
          #trades <- rbind(trades, close.position(candle, curpos, slippage=slippage))
      } else if (curpos < 0) {
        plloc <- plloc - dfred$Ask[r]*abs(curpos)
        numdealsloc <- numdealsloc + abs(curpos)
      }
      restab$pl[i] <- plloc*stepprice/stepval - numdealsloc*commis
      restab$numdeals[i] <- numdealsloc
      restab$maxpos[i] <- maxposeff
    } else {
      if (curpos > 0) {
        pllocalt <- plloc + dfred$Bid[r]*abs(curpos)
        numdealslocalt <- numdealsloc + abs(curpos)
      } else if (curpos < 0) {
        pllocalt <- plloc - dfred$Ask[r]*abs(curpos)
        numdealslocalt <- numdealsloc + abs(curpos)
      }
      restab$pl[i] <- pllocalt*stepprice/stepval - numdealslocalt*commis
      restab$numdeals[i] <- numdealslocalt
      restab$maxpos[i] <- maxposeff
    }
  }
list(restab = restab, pl = sum(restab$pl)/GO, maxpos = max(restab$maxpos))
}
###таже самая стратегия, в которой фигурирует стоплосс
myNetStrategyStopLoss <- function(df, mask, indicator, maxpos, slippage, params) {
  options(stringsAsFactors=FALSE)
  curpos <- 0
  trades <- create.trades()
  trade <- c()
  stoplossmode <- 0
  if (is.null(df$Bid)) {df$Bid <- df$Close}
  if (is.null(df$Ask)) {df$Ask <- df$Close}
  for(r in 1:nrow(df)) {
    candle = df[r, ]
    modshift <- -curpos * params$shift[r]
    if ((!is.na(indicator[r])) & (mask[r]>0) & (!is.na(params$size[r]))) {
      if (mask[r]<2) {

        if (curpos != 0) {
          verdict <- StopLossCheckAlt(trade, candle, params$stoploss[r])
          if (verdict > 0) {
            trade <- buy(candle, slippage = slippage)
            curpos <- curpos + 1
            trades <- rbind(trades, trade)
            stoplossmode <- 1
          } else if (verdict < 0) {
            trade <- sell(candle, slippage = slippage)
            curpos <- curpos - 1
            trades <- rbind(trades, trade)
            stoplossmode <- -1
          }
        }

        if ((stoplossmode == 1) & (df$Ask[r] < indicator[r] + params$size[r] + modshift)) {
          stoplossmode <- 0
        } else if ((stoplossmode == -1) & (df$Bid[r] > indicator[r] - params$size[r] + modshift)) {
          stoplossmode <- 0
        }

        if (df$Bid[r] > indicator[r] + params$size[r] + modshift - 0.00001) {
          if ((curpos > -maxpos) & (stoplossmode == 0)) {
            trade <- sell(candle, slippage=slippage, limit.price = indicator[r] + params$size[r] + modshift - 0.00001)
            curpos <- curpos - 1
            trades <- rbind(trades, trade)
          }
        } else if (df$Ask[r] < indicator[r] - params$size[r] + modshift + 0.00001) {
          if ((curpos < maxpos) & (stoplossmode == 0)) {
            trade <- buy(candle, slippage=slippage, limit.price = indicator[r] - params$size[r] + modshift + 0.00001)
            curpos <- curpos + 1
            trades <- rbind(trades, trade)
          }
        }
      } else if (mask[r]==2) {
        stoplossmode <- 0
        if (curpos != 0) {
          trades <- rbind(trades, close.position(candle, curpos, slippage=slippage))
        }
        curpos = 0
      }
    }
  }
  trades
}
