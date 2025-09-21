###control = 1, only index model
###params should have: size, shift, sizeind, stoploss, takeprofit, dur
Strategy_MMVBArbitrageWithInd <- function(tab, params, delay = 0, regime = "standard") {
  params$control <- 2
  if (!("id" %in% names(tab))) {tab$id <- 1:nrow(tab)}
  dates <- unique(tab$date)
  restab <- data.frame(date = rep(as.Date("1970-01-01"), length(dates)),
                       modelprofitbuy = rep(NA, length(dates)),
                       modelprofitsell = rep(NA, length(dates)),
                       maxposes = rep(NA, length(dates)),
                       numbuy = rep(NA, length(dates)),
                       numsell = rep(NA, length(dates)),
                       cumwaittime = rep(NA, length(dates)))
  modelprofit <- 0
  curpos <- 0
  varshift <- 0

  mid <- median(tab$modelind)
  tab$modelind <- tab$modelind - mid
  memtab <- data.frame(ind = numeric(), price = numeric(),
                       dir = numeric(), flag = numeric(), id = numeric())

  if (regime == "standard") {
    tab$zeroask <- tab$modelask
    tab$zerobid <- tab$modelbid
  } else {
    tab$zeroask <- (tab$modelask + tab$modelbid)/2
    tab$zerobid <- tab$zeroask
  }

  for (i in seq_along(dates)) {
    date <- dates[i]
    tabred <- tab[tab$mix.date == date, ]
    dayresultbuy <- 0
    dayresultsell <- 0
    daynumbuy <- 0
    daynumsell <- 0
    daycumwaittime <- 0
    print(date)
    maxpos <- abs(curpos)
    for (j in 1:nrow(tabred)) {
      if (j <= delay) {next}
      if (j %% 1000 == 0) {print(j)}
      if (params$control == 2) {
        sizeind <- params$sizeind
        size <- params$size
        shift <- params$shift
        if ((tabred$modelind[j] >= sizeind) & (tabred$mix.bid[j] - tabred$zeroask[j-delay] >= varshift + size)) {
          dealval <- tabred$mix.bid[j] - tabred$modelask[j-delay]
          dayresultbuy <- dayresultbuy + dealval
          daynumbuy <- daynumbuy + 1
          varshift <- varshift + shift
          curpos <- curpos - 1
          memtab <- rbind(memtab, data.frame(ind = j,
                                             price = dealval,
                                             dir = 1,
                                             flag = 0,
                                             id = tabred$id[j]))
        } else if ((tabred$modelind[j] <= -sizeind) & (tabred$zerobid[j-delay] - tabred$mix.ask[j] >= size - varshift)) {
          dealval <- tabred$modelbid[j-delay] - tabred$mix.ask[j]
          dayresultsell <- dayresultsell + dealval
          daynumsell <- daynumsell + 1
          varshift <- varshift - shift
          curpos <- curpos + 1
          memtab <- rbind(memtab, data.frame(ind = j,
                                             price = dealval,
                                             dir = -1,
                                             flag = 0,
                                             id = tabred$id[j]))
        }
        if (nrow(memtab) > 0) {
          memtabinfo <- AnalyzeSavedDeals(j, biddev = tabred$mix.bid[j] - tabred$modelask[j-delay],
                                          askdev = tabred$modelbid[j-delay] - tabred$mix.ask[j],
                                          memtab, params, id = tabred$id[j])
          curpos <- curpos + memtabinfo$curpos
          dayresultbuy <- dayresultbuy + memtabinfo$dayresultbuy
          dayresultsell <- dayresultsell + memtabinfo$dayresultsell
          daycumwaittime <- daycumwaittime + memtabinfo$daycumwaittime
          memtab <- memtabinfo$memtab
        }
      }
    }

    if ((tabred$daystillexpir[nrow(tabred)] == 6) | (i == length(dates))) {
      if (curpos>0) {
        dayresultsell <- dayresultsell + abs(curpos)*(tabred$mix.bid[j] - tabred$modelask[j])
      } else {
        dayresultbuy <- dayresultbuy + abs(curpos)*(tabred$modelbid[j] - tabred$mix.ask[j])
      }
      if (nrow(memtab) > 0) {
        daycumwaittime <- daycumwaittime + sum(tabred$id[nrow(tabred)] - memtab$id)
      }
      memtab <- data.frame(ind = numeric(), price = numeric(),
                           dir = numeric(), flag = numeric(), id = numeric())
      curpos <- 0
    }
    restab$date[i] <- date
    restab$modelprofitbuy[i] <- dayresultbuy
    restab$modelprofitsell[i] <- dayresultsell
    restab$maxposes[i] <- curpos
    restab$numbuy[i] <- daynumbuy
    restab$numsell[i] <- daynumsell
    restab$cumwaittime[i] <- daycumwaittime
  }
restab
}
AnalyzeSavedDeals <- function(j, biddev, askdev, memtab, params, id) {
  dayresultsell <- 0
  dayresultbuy <- 0
  daycumwaittime <- 0
  curpos <- 0
  for (k in 1:nrow(memtab)) {
    valtowrite <- NA
    markme <- FALSE
    val <- ifelse(memtab$dir[k]>0, askdev, biddev)
    if (j - memtab$ind[k] >= params$dur) {
      ###exit by time
      markme <- TRUE
      valtowrite <- val
    } else if (val <= -memtab$price[k] - params$stoploss) {
      ###exit by stoploss
      print("stoploss")
      ###not active currently
      valtowrite <- -memtab$price[k] - params$stoploss
      markme <- TRUE
    } else if (val >= -memtab$price[k] + params$takeprofit) {
      ###exit by takeprofit
      valtowrite <- -memtab$price[k] + params$takeprofit
      markme <- TRUE
    }
    if (markme) {
      memtab$flag[k] <- NA
      if (memtab$dir[k] > 0) {
        curpos <- curpos + 1
        dayresultbuy <- dayresultbuy + valtowrite
      } else if (memtab$dir[k] < 0){
        curpos <- curpos - 1
        dayresultsell <- dayresultsell + valtowrite
      }
      daycumwaittime <- daycumwaittime + id - memtab$id[k]
    }
  }
list(memtab = na.omit(memtab), curpos = curpos,
     dayresultbuy = dayresultbuy, dayresultsell = dayresultsell,
     daycumwaittime = daycumwaittime)
}
