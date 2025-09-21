GetSberSpreads <- function() {
  tab1 <- GetCleanData("spot", "SRU7@FORTS", datestart = as.Date("2017-06-15"),
                       dateend = Sys.Date(), candletype = "DataVol4", timelen = 1,
                       storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\")
  tab2 <- GetCleanData("spot", "SPU7@FORTS", datestart = as.Date("2017-06-15"),
                       dateend = Sys.Date(), candletype = "DataVol4", timelen = 1,
                       storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\")
  list("sr" = tab1, "sp" = tab2)
}
PlotIndex <- function(reportpath, id, tabspreads, control, abarplot = FALSE) {
  res1 <- GroupWarDeals(reportpath, id)
  res2 <- DealByDealMatchingInsideDayWar(res1)
  tabspreads[, modelask := sp.ask - 0.75*sr.bid]
  tabspreads[, modelbid := sp.ask - 0.75*sr.bid]
  print(summary(tabspreads))
  #stop()
  vals <- numeric(nrow(res2))
  for (j in 1:nrow(res2)) {
    time1 <- res2$Time1[j]
    time2 <- res2$Time2[j]
    if (res2$Direction1[j] == "Sell") {
      quotes <- tabspreads[(Time >= time1) & (Time <= time2), modelask]
      print(summary(quotes))
      if (control == "MFE") {
        val <- res2$Price1[j] - min(quotes)
      } else if (control == "MAE") {
        val <- res2$Price1[j] - max(quotes)
      }
    } else {
      quotes <- tabspreads[(Time >= time1) & (Time <= time2), modelbid]
      if (control == "MFE") {
        val <- res2$Price1[j]+max(quotes)
      } else if (control == "MAE") {
        val <- res2$Price1[j]+min(quotes)
      }
    }
    vals[j] <- val
  }
  if (abarplot) {
    boxplot(vals~as.Date(res2$Time1))
  } else {
    plot(res2$Time1, vals)
    abline(h = 0, col = "red")
  }
}
PlotEquity <- function(basereport, id) {
  report <- paste0(basereport, "IterationsResult\\", DefineFileById(id, "curves.csv"))
  tab <- fread(report, sep = ';')
  tab[, Time := fastPOSIXct(Time)]
  plot(tab$Time, tab$PL, type = "l")
}
CreateMatchedDF <- function(time = as.POSIXct(numeric(), origin = origin),
                            Price = numeric(),
                            Direction = character()) {
  data.frame(Time = time, Price = Price, Direction = Direction)
}
CreateFullyMatchedDf <- function(time1 = as.POSIXct(numeric(), origin = origin),
                                 time2 = as.POSIXct(numeric(), origin = origin),
                                 Price1 = numeric(), Price2 = numeric(),
                                 Direction1 = character(), Direction2 = character()) {
  data.frame(Time1 = time1, Price1 = Price1, Direction1 = Direction1,
             Time2 = time2, Price2 = Price2, Direction2 = Direction2)
}
CreatePreMatchedFrame <- function(atime = as.POSIXct(numeric(), origin = origin),
                                  price = numeric(), volume = numeric()) {
  data.frame(dateTime = atime, Volume = volume, spread = price)
}
GroupWarDeals <- function(basereport, id, sec1 = "SPU7@FORTS", sec2 = "SRU7@FORTS") {
  report <- paste0(basereport, "IterationsResult\\", DefineFileById(id, "trades.csv"))
  tab <- fread(report, sep = ';')
  tab[, Time := fastPOSIXct(Time)]
  print(head(tab))
  tab[, matched := FALSE]
  res <- CreatePreMatchedFrame()
  tab[Direction == "Sell", Volume := -Volume]
  tab[, Comment := as.numeric(substr(Comment, 3, nchar(Comment)))]
  print(head(tab))
  for (j in 1:nrow(tab)) {
    if (!tab$matched[j]) {
      i <- j+1
      mycomment <- tab[j, Comment]
      while (TRUE) {
        if (i > nrow(tab)) {break}
        if (tab[i, Comment] != mycomment) {break}
        i <- i + 1
      }
      atime <- tab$Time[j]
#      tabred <- tab[(Time >= atime) & (Time <= atime + 1), ]
      tabred <- tab[j:(i-1), ]
      vol1 <- tabred[Security == sec1, sum(Volume)]
      price1 <- tabred[Security == sec1, -sum(Volume*Price)/sum(Volume)]
      vol2 <- tabred[Security == sec2, sum(Volume)]
      price2 <- tabred[Security == sec2, -sum(Volume*Price)/sum(Volume)]
      #tab[(Time >= atime) & (Time <= atime + 1), matched := TRUE]
      tab[j:(i-1), matched := TRUE]
      print(paste(atime, vol1, vol2, price1, price2))
      res <- rbind(res, CreatePreMatchedFrame(atime, (vol1*price1+vol2*price2)/abs(vol1), vol1))
    }
  }
  res$comvol <- cumsum(res$Volume)
return(res)
}
PlotGroupedDeals <- function(restab, abarplot) {
  #restab[, res := ifelse(Direction1 == "Sell", Price1-Price2, Price2-Price1)]
  restab[, res := Price1+Price2]
  restab[, date := as.Date(Time1)]
  restab <- restab[is.finite(res), ]
  print(summary(restab))
  if (abarplot) {
    boxplot(res~date, data = restab)
  } else {
    plot(restab$date, restab$res)
    abline(h = 0, col = "red")
  }
}
PlotById <- function(reportpath, id, abarplot = FALSE) {
  res1 <- GroupWarDeals(reportpath, id)
  res2 <- DealByDealMatchingInsideDayWar(res1)
#  return(res1)
  PlotGroupedDeals(as.data.table(res2), abarplot = abarplot)
}
DealByDealMatchingInsideDayWar <- function(tab) {
  tab <- as.data.table(as.data.frame(tab))
  tab$Volume <- tab$comvol/4
  tab[, Date := as.Date(dateTime)]
  dates <- unique(tab$Date)
  resmatcheddf <- CreateFullyMatchedDf()
  for (adate in dates) {
    print(as.Date(adate))
    tabred <- tab[Date == adate, ]
    poscur <- 0
    matcheddf <- CreateMatchedDF()
    for (j in 1:nrow(tabred)) {
      pricenew <- tabred$spread[j]
      timenew <- tabred$dateTime[j]
      posnew <- tabred$Volume[j]
      if (poscur == 0) {
        #prices <- c(prices, rep(pricenew, abs(posnew)))
        dirnew <- ifelse(sign(posnew)>0, "Buy", "Sell")
        for (count in 1:abs(posnew)) {
          matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
        }
        poscur <- posnew
      } else if (poscur > 0) {
        if (posnew > poscur) {
          dirnew <- "Buy"
          for (count in 1:abs(posnew-poscur)) {
            matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
          }
          poscur <- posnew
        } else if (posnew < poscur) {
          dirnew <- "Sell"
          for (count in 1:abs(posnew-poscur)) {
            if (nrow(matcheddf)>0) {
              resmatcheddf <- rbind(resmatcheddf,
                                    CreateFullyMatchedDf(matcheddf$Time[1],
                                                         timenew,
                                                         matcheddf$Price[1],
                                                         pricenew,
                                                         matcheddf$Direction[1],
                                                         dirnew))
              matcheddf <- matcheddf[-1, ]
            } else {
              matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
            }
          }
          poscur <- posnew
        } else {
          print(j)
          print("Strange situation 1")
          next
        }
      } else if (poscur < 0) {
        if (posnew < poscur) {
          dirnew <- "Sell"
          for (count in 1:abs(posnew-poscur)) {
            matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
          }
          poscur <- posnew
        } else if (poscur < posnew) {
          dirnew <- "Buy"
          for (count in 1:abs(posnew-poscur)) {
            if (nrow(matcheddf)>0) {
              resmatcheddf <- rbind(resmatcheddf,
                                    CreateFullyMatchedDf(matcheddf$Time[1],
                                                         timenew,
                                                         matcheddf$Price[1],
                                                         pricenew,
                                                         matcheddf$Direction[1],
                                                         dirnew))
              matcheddf <- matcheddf[-1, ]
            } else {
              matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
            }
          }
          poscur <- posnew
        } else {
          print(j)
          print("Strange situation 3")
          next
        }
      } else {
        stop("Strange situation 2")
      }
    }
  }
  resmatcheddf
}
DealByDealMatchingInsideDay <- function(basereport, id) {
  report <- paste0(basereport, "IterationsResult\\", DefineFileById(id, ""))
  files <- list.files(report)
  afile <- files[grepl('@', files)]
  print(afile)
  tab <- fread(paste0(report, afile), sep = ';')
  tab[, dateTime := as.POSIXct(dateTime, format = "%d-%m-%Y %H:%M:%S.%OS")]
  tab[, Date := as.Date(dateTime)]
  dates <- unique(tab$Date)
  resmatcheddf <- CreateFullyMatchedDf()
  for (adate in dates) {
    tabred <- tab[Date == adate, ]
    poscur <- 0
    matcheddf <- CreateMatchedDF()
    for (j in 1:nrow(tabred)) {
      pricenew <- tabred$spread[j]
      timenew <- tabred$dateTime[j]
      posnew <- tabred$IndicatorCalculator.TargetSinteticPosition[j]
      if (poscur == 0) {
        #prices <- c(prices, rep(pricenew, abs(posnew)))
        dirnew <- ifelse(sign(posnew)>0, "Buy", "Sell")
        for (count in 1:abs(posnew)) {
          matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
        }
        poscur <- posnew
      } else if (poscur > 0) {
        if (posnew > poscur) {
          dirnew <- "Buy"
          for (count in 1:abs(posnew-poscur)) {
            matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
          }
          poscur <- posnew
        } else if (posnew < poscur) {
          dirnew <- "Sell"
          for (count in 1:abs(posnew-poscur)) {
            if (nrow(matcheddf)>0) {
              resmatcheddf <- rbind(resmatcheddf,
                                    CreateFullyMatchedDf(matcheddf$Time[1],
                                                         timenew,
                                                         matcheddf$Price[1],
                                                         pricenew,
                                                         matcheddf$Direction[1],
                                                         dirnew))
              matcheddf <- matcheddf[-1, ]
            } else {
              matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
            }
          }
          poscur <- posnew
        } else {
          stop("Strange situation 1")
        }
      } else if (poscur < 0) {
        if (posnew < poscur) {
          dirnew <- "Sell"
          for (count in 1:abs(posnew-poscur)) {
            matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
          }
          poscur <- posnew
        } else if (poscur < posnew) {
          dirnew <- "Buy"
          for (count in 1:abs(posnew-poscur)) {
            if (nrow(matcheddf)>0) {
              resmatcheddf <- rbind(resmatcheddf,
                                    CreateFullyMatchedDf(matcheddf$Time[1],
                                                         timenew,
                                                         matcheddf$Price[1],
                                                         pricenew,
                                                         matcheddf$Direction[1],
                                                         dirnew))
              matcheddf <- matcheddf[-1, ]
            } else {
              matcheddf <- rbind(matcheddf, CreateMatchedDF(timenew, pricenew, dirnew))
            }
          }
          poscur <- posnew
        } else {
          stop("Strange situation 3")
        }
      } else {
        stop("Strange situation 2")
      }
    }
  }
resmatcheddf
}
