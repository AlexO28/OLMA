StrangeStrategyvarKefs <- function(tab, params, model, gos, control, withtrades = FALSE) {
  dates <- model$Date
  restab <- data.frame(date = rep(as.Date("1970-01-01"), length(dates)),
                       modelprofit = rep(NA, length(dates)),
                       lostwithmg = rep(NA, length(dates)),
                       frozenmoney = rep(NA, length(dates)),
                       maxpos = rep(NA, length(dates)))
  trades <- CreateTrade()
  modelprofit <- 0
  lostwithspread <- 0
  lostwithmg <- 0
  curpos <- 0
  varshift <- 0

  for (j in 1:nrow(model)) {
    Date <- model$Date[j]
    print(Date)
    tabred <- tab[tab$mix.date == Date, ]
    size <- params$size
    shift <- params$shift

    daymodel <- data.frame(bid = rep(NA, nrow(tabred)), ask = (rep(NA, nrow(tabred))))
    if (control == 1) {
      daymodel$bid <- model$Intercept[j] + model$gaz[j]*tabred$gaz.bid +
        model$lokh[j]*tabred$lokh.bid + model$sber[j]*tabred$sber.bid
      daymodel$ask <- model$Intercept[j] + model$gaz[j]*tabred$gaz.ask +
        model$lokh[j]*tabred$lokh.ask + model$sber[j]*tabred$sber.ask
    } else if (control == 2) {
      daymodel$bid <- model$Intercept[j] + model$gaz[j]*tabred$gaz.bid +
        model$lokh[j]*tabred$lokh.bid
      daymodel$ask <- model$Intercept[j] + model$gaz[j]*tabred$gaz.ask +
        model$lokh[j]*tabred$lokh.ask
    } else if (control == 3) {
      daymodel$bid <- model$Intercept[j] + model$gaz[j]*tabred$gaz.bid
      daymodel$ask <- model$Intercept[j] + model$gaz[j]*tabred$gaz.ask
    }
    if (curpos == 0) {
      mg <- 0
    } else if (curpos > 0) {
      if (control == 1) {
        mg <- curpos*(model$Intercept[j-1] + model$gaz[j-1]*tabred$gaz.bid[1] +
           model$lokh[j-1]*tabred$lokh.bid[1] + model$sber[j-1]*tabred$sber.bid[1] - daymodel$ask[1])
      } else if (control == 2) {
        mg <- curpos*(model$Intercept[j-1] + model$gaz[j-1]*tabred$gaz.bid[1] +
                        model$lokh[j-1]*tabred$lokh.bid[1] - daymodel$ask[1])
      } else if (control == 3) {
        mg <- curpos*(model$Intercept[j-1] + model$gaz[j-1]*tabred$gaz.bid[1] - daymodel$ask[1])
      }
    } else {
      if (control == 1) {
        mg <- abs(curpos)*(daymodel$bid[1] - (model$Intercept[j-1] + model$gaz[j-1]*tabred$gaz.ask[1] +
                                 model$lokh[j-1]*tabred$lokh.ask[1] + model$sber[j-1]*tabred$sber.ask[1]))
      } else if (control == 2) {
        mg <- abs(curpos)*(daymodel$bid[1] - (model$Intercept[j-1] + model$gaz[j-1]*tabred$gaz.ask[1] +
                                           model$lokh[j-1]*tabred$lokh.ask[1]))
      } else if (control == 3) {
        mg <- abs(curpos)*(daymodel$bid[1] - (model$Intercept[j-1] + model$gaz[j-1]*tabred$gaz.ask[1]))
      }
    }
    dayresult <- 0
    maxpos <- curpos
    for (i in 1:nrow(tabred)) {
      if (tabred$mix.bid[i] >= daymodel$ask[i] + size + varshift) {
        curpos <- curpos + 1
        varshift <- varshift + shift
        dayresult <- dayresult + size + varshift
      } else if (tabred$mix.ask[i] <= daymodel$bid[i] - size + varshift) {
        curpos <- curpos - 1
        varshift <- varshift - shift
        dayresult <- dayresult - (size - varshift)
      }
      if (maxpos < abs(curpos)) {maxpos <- abs(curpos)}
    }
    if (control == 1) {
      frozen <- max(gos$mix, abs(model$gaz[j])*gos$gaz + abs(model$lokh[j])*gos$lokh + abs(model$sber[j])*gos$sber)
    } else if (control == 2) {
      frozen <- max(gos$mix, abs(model$gaz[j])*gos$gaz + abs(model$lokh[j])*gos$lokh)
    } else if (control == 3) {
      frozen <- max(gos$mix, abs(model$gaz[j])*gos$gaz)
    }
    if ((tabred$daystillexpir[nrow(tabred)] == 6) | (j == nrow(model))) {
      curpos <- 0
      varshift <- 0
      if (curpos > 0) {
        dayresult <- dayresult -  abs(curpos)*shift
      } else {
        dayresult <- dayresult + abs(curpos)*shift
      }
    }
    restab[j, ] <- data.frame(date = Date,
                              modelprofit = dayresult,
                              lostwithmg = mg,
                              frozenmoney = frozen,
                              maxpos = maxpos)
  }
restab
}
