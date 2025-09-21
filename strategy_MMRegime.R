#add delay-parameter
#tab should have column beta if params$beta is NA
#params should have: size, shift, check
Strategy_MMVBMMRegime <- function(tab, params, delay = 0, commis = 0, slippage = 0, regime = "marketmaking", assetname = "mix.") {
  dates <- unique(tab$date)
  restab <- data.frame(date = rep(as.Date("1970-01-01"), length(dates)),
                       modelprofit = rep(NA, length(dates)),
                       curposes = rep(NA, length(dates)),
                       numdeals = rep(NA, length(dates)))
  modelprofit <- 0
  curpos <- 0
  varshift <- 0
  size <- params$size
  shift <- params$shift
  tab$zero <- (tab$modelbid + tab$modelask)/2
  check <- params$check

  for (i in seq_along(dates)) {
    date <- dates[i]
    print(as.Date(date))
    tabred <- tab[tab[, paste0(assetname, "date")] == date, ]
    dayresult <- 0
    daynumdeals <- 0

    bidname <- paste0(assetname, "bid")
    askname <- paste0(assetname, "ask")

    for (j in (1+delay):nrow(tabred)) {
      if (check) {
        if (size < (tabred$modelask[j-delay] - tabred$modelbid[j-delay] + shift)/2) {next}
      }
      if (regime == "marketmaking") {
        conditionsell <- tabred[j, bidname] - (tabred$zero[j-delay] + varshift + size)
        conditionbuy <- tabred[j, askname] - (tabred$zero[j-delay] + varshift - size)
      } else {
        conditionsell <- tabred[j, bidname] - (tabred$modelask[j-delay] + varshift + size)
        conditionbuy <- tabred[j, askname] - (tabred$modelbid[j-delay] + varshift - size)
      }
      if (conditionsell >= 0) {
        ###we sell mmvb and buy the model
        curpos <- curpos - 1
        dayresult <- dayresult + tabred[j, bidname] - tabred$modelask[j] - slippage - commis
        daynumdeals <- daynumdeals + 1
        varshift <- varshift + shift
      } else if (conditionbuy <= 0) {
        curpos <- curpos + 1
        dayresult <- dayresult + tabred$modelbid[j] - tabred[j, askname] - slippage - commis
        varshift <- varshift - shift
        daynumdeals <- daynumdeals + 1
      }
    }
    ###here we close position
    mindayval <- 6
    if ((i == length(dates)) | (tabred$daystillexpir[nrow(tabred)] == mindayval)) {
      if (curpos > 0) {
        curpos <- 0
        dayresult <- dayresult + abs(curpos)*(tabred[j, bidname] - tabred$modelask[j]) - slippage - commis
      } else if (curpos < 0) {
        curpos <- 0
        dayresult <- dayresult + abs(curpos)*(tabred$modelbid[j] - tabred[j, askname]) - slippage - commis
      }
    }
    ###here we add data
    restab$date[i] <- date
    restab$modelprofit[i] <- dayresult
    restab$curposes[i] <- curpos
    restab$numdeals[i] <- daynumdeals
  }
restab
}
