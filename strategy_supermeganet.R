StrangeStrategyForLib <- function(tab, model, params, graphparams=NA, simplemode = FALSE, internal = TRUE, entermode = "both") {
  size <- params$size
  shift <- params$shift
  control <- params$control
  if (!simplemode & internal) {
    catalogname <- graphparams$catalogname
    filename <- graphparams$filename
    jj <- graphparams$j
    num <- graphparams$num
  }
  cash <- 0
  curpos <- 0
  cashesfull <- c()
  curposesfull <- c()
  tradesfull <- rbind(c(), c())
  numbersofdeals <- c()
  numbersofdealsev <- c()
  cashesev <- c()
  modshift <- 0
  difflessesfull <- c()
  diffgreatesfull <- c()

  if (!internal) {
    trades <- data.frame(time  = numeric(), price = numeric(), dir = numeric())
    cashes <- c()
    curposes <- c()
    modelsask <- c()
    modelsbid <- c()

    diffgreates<- c()
    difflesses <- c()
    modelshift <- 0
    numberofdeals <- 0
    numberofdealsev <- 0
    maxposes <- c()
  }
  for (date in unique(model$date)) {
    date <- as.Date(date, tz = "UTC", origin = "1970-01-01")
    print(date)
    cashev <- 0
    tabred <- tab[tab$Date == date, ]
    if (internal) {
      trades <- data.frame(time  = numeric(), price = numeric(), dir = numeric())
    }
    if (internal) {
      cashes <- c()
      curposes <- c()
      modelsask <- c()
      modelsbid <- c()
    }
    modelred <- model[model$date == date, ]
    if (internal) {
      diffgreates<- c()
      difflesses <- c()
      modelshift <- 0
      numberofdeals <- 0
      numberofdealsev <- 0
      maxposes <- c()
    }
    diffgreates<- rep(NA, nrow(tabred))
    difflesses <- rep(NA, nrow(tabred))
    modelsbid <- rep(NA, nrow(tabred))
    modelsask <- rep(NA, nrow(tabred))
    curposes <- rep(NA, nrow(tabred))
    cashes <- rep(NA, nrow(tabred))
    print(nrow(trades))
  for (j in 1:nrow(tabred)) {
      modelbid <- modelred$modelbid[j]
      modelask <- modelred$modelask[j]
      modelsbid[j] <- modelbid
      modelsask[j] <- modelask
      if (control == 1) {
        diffless <- tabred$Bid[j] - modelask
        diffgreat <- -modelbid+tabred$Ask[j]
      } else {
        diffless <- min(tabred$Bid[j] - modelbid, tabred$Ask[j] - modelask)
        diffgreat <- max(tabred$Bid[j] - modelbid, tabred$Ask[j] - modelask)
      }
#      difflesses <- c(difflesses, diffless)
#      diffgreates <- c(diffgreates, diffgreat)
      difflesses[j] <- diffless
      diffgreates[j] <- diffgreat
      if (diffless - modshift*shift > size) {
        if (curpos>0 | entermode != "buy") {
          price <- size + modshift*shift
          cash <- cash + price
          trades <- rbind(trades, data.frame(time = tabred$Time[j], price = price, dir = -1))
          curpos <- curpos - 1
          modshift <- modshift + 1
          numberofdeals <- numberofdeals + 1
          if (tabred$Time[j] > as.POSIXct(paste(date, "19:00:00"), tz = "UTC")) {
            numberofdealsev <- numberofdealsev + 1
            cashev <- cashev + price
          }
        }
      } else if (modshift*shift - diffgreat > size) {
        if (curpos<0 | entermode != "sell") {
          price <- - size + modshift*shift
          cash <- cash - price
          trades <- rbind(trades, data.frame(time = tabred$Time[j], price = price, dir = 1))
          curpos  <- curpos + 1
          modshift <- modshift - 1
          numberofdeals <- numberofdeals + 1
          if (tabred$Time[j] > as.POSIXct(paste(date, "19:00:00"), tz = "UTC")) {
            numberofdealsev <- numberofdealsev + 1
            cashev <- cashev + price
          }
        }
      }
#      curposes <- c(curposes, curpos)
      curposes[j] <- curpos
      if (curpos>0) {
        #cashes <- c(cashes, cash + diffless*curpos)
        cashes[j] <- cash + diffless*curpos
      } else if (curpos<0) {
        #cashes <- c(cashes, cash + diffgreat*curpos)
        cashes[j] <- cash + diffgreat*curpos
      } else {
        #cashes <- c(cashes, cash)
        cashes[j] <- cash
      }
    }
    difflessesfull <- c(difflessesfull, difflesses)
    diffgreatesfull <- c(diffgreatesfull, diffgreates)
    names(trades) <- c("time", "price", "dir")
    trades$time <- as.POSIXct(trades$time, origin = "1970-01-01", tz = "UTC")
    tradesfull <- rbind(tradesfull, trades)
    numbersofdealsev <- c(numbersofdealsev, numberofdealsev)
    numbersofdeals <- c(numbersofdeals, numberofdeals)
    cashesfull <- c(cashesfull, cashes[length(cashes)])
    cashesev <- c(cashesev, cashev)
    curposesfull <- c(curposesfull, curposes[length(curposes)])
    maxposes <- c(maxposes, max(curposes, na.rm = TRUE))
    ###а здесь мы рисуем бесполезные графики
    if (!simplemode & internal) {
      PlotUselessGraphs(tabred, difflesses, diffgreates, trades, curposes, cashes, size, jj, date, catalogname, paste0(filename, control))
    }
  }
  if (!simplemode) {
    if (internal) {
      temptab <- tab[tab$Date %in% model$date, ]
      PlotUselessGraphs(temptab, difflessesfull, diffgreatesfull, tradesfull, curposesfull, cashesfull, size, jj, "total",
                      paste0(catalogname, "total"), paste0(filename, control))
    }
    res <- list(trades = tradesfull, numbersofdealsev = numbersofdealsev, numbersofdeals = numbersofdeals, cashes = cashesfull,
                cashesev = cashesev, curposes = curposesfull, maxposes = maxposes,
                difflesses = difflessesfull, diffgreates = diffgreatesfull)
  } else {
    res <- tradesfull
  }
  res
}

PlotUselessGraphs <- function(tabMIX, difflesses, diffgreates, trades, curposes, cashes, size, jj, date, catalogname, filename) {
  dir.create(catalogname)
  filenamenew <- paste0(filename,"_", jj,"_", size, "_", date, "_", "_net.jpeg")
  jpeg(file.path(catalogname, filenamenew), width = 2000, height = 1000)
  par(mfcol = c(3,1))
  plot(tabMIX$Time, difflesses, type = "l", col = "blue", ylim = c(-500, 500))
  lines(tabMIX$Time, diffgreates, col = "black")
  mulsize <- size
  abline(h=0)
  while (mulsize < 500) {
    abline(h = mulsize)
    abline(h = -mulsize)
    mulsize <- mulsize + size
  }
  if (nrow(trades)>0) {
    for (k in 1:nrow(trades)) {
      if (trades$dir[k] > 0) {
        points(as.POSIXct(trades$time[k], origin = "1970-01-01"), trades$price[k], pch=24, bg = "green", col="green", cex=4)
      } else if (trades$dir[k] < 0) {
        points(as.POSIXct(trades$time[k], origin = "1970-01-01"), trades$price[k], pch=25, bg = "red", col="red", cex=4)
      }
    }
    plot(tabMIX$Time, curposes, type = "l")
    plot(tabMIX$Time, cashes, type = "l")
  }
  dev.off()
}
