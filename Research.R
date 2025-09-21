library(TTR)

GetModelForTwoFutures <- function(datestart, dateend, n, mod, curspotname, curfutname) {
  tab <- FillStavkaTab(datestart, dateend, spotsec = curspotname, futsec = curfutname,
                                candletype = "Data", storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\", mod = mod)
  tab[, avmodel := EMA(stavka, n)]
  tab[, model := mod*(stavka-avmodel)]
  tab[, Date := as.Date(Time)]
}

FillLinearTrend <- function(tab, critdate, alpha1, alpha2) {
  tab[, daysdiff := as.numeric(critdate) - as.numeric(si.date)]
  tab[, lintrend := alpha1*daysdiff + alpha2]
}
DefineDatesStruct <- function(tab, datecrit, datenext) {
  dates <- unique(tab$Date)
  diffs <- as.numeric(datecrit) - as.numeric(dates)
  datestruct <- data.table(dates = dates, diffs = diffs)
  datestruct$newdiffs <- c(datestruct$diffs[2:nrow(datestruct)],
                           as.numeric(datecrit) - as.numeric(datenext))
setkey(na.omit(datestruct), "dates")
}
FillModelInfo <- function(tab, datestruct) {
  tabnew <- tab
  setkey(tabnew, "Date")
tabnew[datestruct]
}
PrepareModel <- function(tab, critdate, nextdate) {
  tab[, Date := si.date]
  tab <- FillLinearTrend(tab, critdate, NA, NA)
  datestruct <- DefineDatesStruct(tab, critdate, nextdate)
  tab <- FillModelInfo(tab, datestruct)
}
FillShortModel <- function(tabold, len) {
  tab <- tabold
  tab[, avstavka := (stavkabid + stavkaask)/(2*newdiffs)]
  tab[, avstavkashort <- NA]
  dates <- unique(tab$Date)
  for (adate in dates) {
    tabday <- tab[Date == adate, ]
    val <- mean(tabday$avstavka[1:len])
    tab[Date == adate, avstavkashort := val]
  }
  tab
}
GetNewestAnnStavkaAndEtc <- function(tab, len, datefutur, critval) {
  dates <- unique(tab$si.date)
  dates <- dates[max(1, length(dates)-len+1):length(dates)]
  tabred <- tab[si.date %in% dates, ]
  annval <- tabred[, mean(stavka/newdiffs)]
  diffval <- as.numeric(as.Date(critval)) - as.numeric(as.Date(datefutur))
  print(c(annval, diffval))
-1000*annval*diffval
}
FillAnnualizedStavkaInfo <- function(tabold, len, control = 0) {
  tab <- tabold
  tab[, lintrend := NULL]
  tab[, stavka := (stavkabid + stavkaask)/2]
  dates <- unique(tab$Date)
  vals <- numeric(length(dates))
  vals[1] <- NA
  for (j in 2:length(dates)) {
    adate <- as.Date(dates[j])
    adatestart <- as.Date(dates[max(1, j-len)])
    if (control == 0) {
      val <- tab[Date < adate & Date >= adatestart, mean(stavka/newdiffs)]
    } else if (control == 1) {
      val <- tab[Date < adate & Date >= adatestart, mean(stavka/(newdiffs*dol.close))]
    }
    vals[j] <- val
  }
  valtab <- data.table(date = dates, avannmodel = vals)
  setkey(tab, "Date")
  setkey(valtab, "date")
tab[valtab]
}
FillAvStavkas <- function(traintab) {
  traintab[, avstavka1 := stavka/newdiffs]
  traintab[, avstavka2 := stavka/(newdiffs*dol.close)]
}
FillMean <- function(tab, colname, num, mode = "simple") {
  tab <- as.data.table(as.data.frame(tab))
  tab$lintrend <- NULL
  tab <- na.omit(tab)
  print(nrow(tab))
  if (mode == "simple") {
    tab[, paste0(colname, "simpl") := SMA(tab[, colname, with = FALSE], num), with = FALSE]
  } else if (mode == "exponential") {
    tab[, paste0(colname, "exp") := EMA(tab[, colname, with = FALSE], num), with = FALSE]
  }
tab
}
DoublePlot <- function(tab, col1, col2) {
  plot(1:nrow(tab), tab[, col1, with = FALSE][[1]])
  points(1:nrow(tab), tab[, col2, with = FALSE][[1]], col = "red")
}
MakeReal <- function(tab, colname, withdollar = FALSE) {
  tab <- as.data.table(as.data.frame(tab))
  if (!withdollar) {
    tab[, paste0(colname, "real") := tab[, colname, with = FALSE]*tab$newdiffs, with = FALSE]
  } else {
    tab[, paste0(colname, "real") := tab[, colname, with = FALSE]*tab$newdiffs*tab$dol.close, with = FALSE]
  }
tab
}
MakeShortTab <- function(tab, kef = -1, aname = "avstavka1expreal") {
  ktab <- data.frame(Date = tab$Date, Val = tab[, aname, with = FALSE])
  names(ktab) <- c("Date", "Val")
  ktab2 <- aggregate(ktab$Val, list(ktab$Date), function(x) {x[1]})
  ktab2[, 2] <- kef*ktab2[, 2]
ktab2
}
ResearchDataFast <- function(datestart, dateend, curspotname = "USD000UTSTOM@CETS", curfutname, critdate, nextdate, n, modelnum = 1, mod = 1000) {
  tab <- FillStavkaTab(datestart, dateend, spotsec = curspotname, futsec = curfutname,
                       candletype = "Data", storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\", mod = mod)
  tab <- PrepareModel(tab, critdate, nextdate)
  tab[Date == as.Date("2016-09-02"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2016-10-07"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2016-11-03"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2016-11-10"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2016-11-23"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2016-12-23"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2016-12-30"), newdiffs := 66]
  tab[Date == as.Date("2017-01-03"), newdiffs := 66]
  tab[Date == as.Date("2017-01-04"), newdiffs := 66]
  tab[Date == as.Date("2017-01-05"), newdiffs := 66]
  tab[Date == as.Date("2017-01-06"), newdiffs := 66]
  tab[Date == as.Date("2017-01-16"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2017-02-20"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2017-02-24"), newdiffs := newdiffs - 3]

  tab[Date == as.Date("2017-05-26"), newdiffs := newdiffs - 1]
  tab[Date == as.Date("2017-06-30"), newdiffs := newdiffs - 1]


  if (is.na(n)) {
    return(tab)
  }

  tab <- FillAvStavkas(tab)
  print(head(tab))

  if (modelnum == 1) {
    tab <- FillMean(tab, "avstavka1", num = n, mode = "exponential")
    tab <- MakeReal(tab, "avstavka1exp")
  } else {
    tab <- FillMean(tab, "avstavka2", num = n, mode = "exponential")
    tab <- MakeReal(tab, "avstavka2exp", withdollar = TRUE)
  }
tab
}
GetValsForNinthPair <- function(tab, alpha = 15.93343, withmean = FALSE) {
  if (withmean) {
    tab <- na.omit(tab)
  }
  firstfun <- function(x) {x[1]}
  shorttab <- tab[, firstfun(newdiffs), by = list(Date)]
  shorttabreal <- tab[, firstfun(stavka), by = list(Date)]
  names(shorttab) <- c("Date", "newdiffs")
  names(shorttabreal) <- c("Date", "Val")
  shorttab <- shorttab[, Val := alpha*newdiffs]
  if (withmean) {
    shorttabmean <- tab[, firstfun(avstavka1expreal), by = list(Date)]
    names(shorttabmean) <- c("Date", "Val")
  }
list(model = shorttab[, list(Date, Val)], real = shorttabreal,
     mean = shorttabmean)
}
CalculateTrueModel <- function(tab) {
  shtab <- MakeShortTab(tab)
  shtab[, 2] <- -shtab[, 2]
  shtab <- data.table(Date = shtab[, 1], Val = shtab[, 2])
  setkey(tab, "Date")
  setkey(shtab, "Date")
  shtab[tab]
}
GetQuantiles <- function(truemodel, control = 1) {
  if (control == 1) {
    setkey(truemodel, Date)
    qtab1 <- truemodel[, quantile(model, 0.95), by = Date]
    qtab2 <- truemodel[, quantile(model, 0.05), by = Date]
    return(data.frame(Date = qtab1$Date, upper = qtab1$V1, lower = qtab2$V1))
  }
  else {
    setkey(truemodel, Date)
    qtab <- truemodel[, quantile(abs(model), 0.95), by = Date]
    return(qtab)
  }
}
FillAdaptiveModel <- function(tab, n) {
  tab <- na.omit(as.data.table(as.data.frame(tab)))
  tab[, model := newdiffs*avstavka1exp]
  tab[, resids := stavka - model]
  tab[, emaresids := EMA(resids, n)]
  tab[, emaabsresids := EMA(abs(resids), n)]
  tab <- na.omit(tab)
  tab[, slidingcontrol := emaresids/emaabsresids]
tab
}
CalculateAdaptiveModel <- function(tab) {
  tab[, newstavka1exp := NA]
  tab[, purestavka := stavka/newdiffs]
  valprev <- 0
  for (j in 1:nrow(tab)) {
    print(c(j, valprev))
    valprev <- abs(tab$slidingcontrol[j])*tab$purestavka[j] +
      (1-abs(tab$slidingcontrol[j]))*valprev
    tab$newstavka1exp[j] <- valprev
  }
tab[, newmodel := newstavka1exp*newdiffs]
}
CalculateSlidingRegressionModel <- function(tab, datestarteff) {
  tab <- as.data.table(as.data.frame(tab))
  dates <- unique(tab[Date >= datestarteff, Date])
  tab$newmodel <- as.numeric(NA)
  for (j in 1:length(dates)) {
    adate <- as.Date(dates[j])
    print(adate)
    tabtemp <- tab[Date < adate, ]
    mylqs <- coef(rlm(tabtemp$stavka ~ tabtemp$newdiffs))
    tab[Date == adate, newmodel := mylqs[1] + mylqs[2]*newdiffs]
  }
tab
}
FillAdaptiveEMA <- function(tab, n, changedates) {
  tab <- as.data.table(as.data.frame(tab))
  tab[, modstavka := stavka/newdiffs]
  tab[, avmodstavka := NA]
  dates <- unique(tab$Date)
  myn <- 0
  for (j in 1:(length(dates)-1)) {
    adate <- as.Date(dates[j])
    print(adate)
    if (adate %in% changedates) {
      myn <- 1
    } else {
      if (myn<n) {
        myn <- myn + 1
      }
    }
    print(myn)
    tabtemp <- tab[Date <= dates[j+1] & Date >= dates[j+1-myn], ]
    tabtemp[, avmodstavka := EMA(modstavka, n = myn*745)]
    print(summary(tabtemp$avmodstavka))
    tab$avmodstavka[tab$Date == dates[j+1]] <- tabtemp$avmodstavka[tabtemp$Date == dates[j+1]]
    print(summary(tab$avmodstavka[tab$Date == dates[j+1]]))
  }
tab
}
FillAdaptiveRegression <- function(tab, minlen, epsthreshold, withintercept = TRUE) {
  dates <- unique(tab$Date)
  if (minlen >= length(dates)) {
    stop("Not enough days for adaptation. Must return a standard linear regression.")
  }
  cumdates <- 1:minlen
  curind <- minlen + 1
#  tab[, id := 1:nrow(tab)]
  restab <- data.frame(Date = as.Date(numeric()), eps = numeric(), changeflag = logical(),
                       intercept = numeric(), slope = numeric())
  while (curind <= length(dates)) {
    #tabprev <- tab[Date %in% as.Date(dates[c(cumdates, curind)]), ]
    tabprev <- tab[Date %in% as.Date(dates[cumdates]), ]
    if (withintercept) {
      reg0 <- coef(lm(tabprev[, stavka] ~ tabprev[, newdiffs]))
    } else {
      reg0 <- coef(lm(tabprev[, stavka] ~ tabprev[, newdiffs] + 0))
    }
    r0 <- CalculateRSqrd(tabprev, reg0, withintercept = withintercept)
    while (is.na(r0)) {
      cumdates <- c(cumdates[1]-1, cumdates)
      tabprev <- tab[Date %in% as.Date(dates[cumdates]), ]
      if (withintercept) {
        reg0 <- coef(lm(tabprev[, stavka] ~ tabprev[, newdiffs]))
      } else {
        reg0 <- coef(lm(tabprev[, stavka] ~ tabprev[, newdiffs] + 0))
      }
      r0 <- CalculateRSqrd(tabprev, reg0, withintercept = withintercept)
    }
    cumdatesalt <- c(cumdates[(length(cumdates)-minlen+2):length(cumdates)], curind)
    tabcur <- tab[Date %in% as.Date(dates[cumdatesalt]), ]
    r1 <- CalculateRSqrd(tabcur, reg0, withintercept = withintercept)
    print(as.Date(dates[curind]))
    print(c(r0, r1))
    print(cumdates)
    print(cumdatesalt)
    eps <- r1/r0
    if (eps<epsthreshold) {
      cumdates <- cumdatesalt
      changeflag <- TRUE
    } else {
      cumdates <- c(cumdates, curind)
      changeflag <- FALSE
    }
    if (withintercept) {
      restab <- rbind(restab, data.frame(Date = as.Date(dates[curind]),
                                       eps = eps,
                                       changeflag = changeflag,
                                       intercept = reg0[1],
                                       slope = reg0[2]))
    } else {
      restab <- rbind(restab, data.frame(Date = as.Date(dates[curind]),
                                         eps = eps,
                                         changeflag = changeflag,
                                         intercept = NA,
                                         slope = reg0[1]))
    }
    curind <- curind + 1
  }
#  val <- as.numeric(reg0[1] + reg0[2]*tabprev$newdiffs[nrow(tabprev)])
#  print(round(val*1000))
  tabprev <- tab[Date %in% as.Date(dates[cumdates]), ]
  if (withintercept) {
    reg0 <- coef(lm(tabprev[, stavka] ~ tabprev[, newdiffs]))
  } else {
    reg0 <- coef(lm(tabprev[, stavka] ~ tabprev[, newdiffs] + 0))
  }
#  val <- as.numeric(reg0[1] + reg0[2]*tabprev$newdiffs[nrow(tabprev)])
#  print(round(val*1000))
  if (withintercept) {
    restab <- rbind(restab, data.frame(Date = Sys.Date(),
                                     eps = NA,
                                     changeflag = NA,
                                     intercept = reg0[1],
                                     slope = reg0[2]))
  } else {
    restab <- rbind(restab, data.frame(Date = Sys.Date(),
                                       eps = NA,
                                       changeflag = NA,
                                       intercept = NA,
                                       slope = reg0[1]))
  }
restab
}
FillRegModelInTab <- function(tab, regtab, datestart, withintercept = TRUE) {
  #mystr <- tab[1, ]
  #mystr$Date <- Sys.Date()
  tab <- as.data.table(as.data.frame(tab))
  regtab <- as.data.table(as.data.frame(regtab))
  dates <- unique(regtab$Date)
  tab <- tab[Date %in% dates, ]
  for (adate in as.Date(dates[dates >= as.Date(datestart)])) {
    adate <- as.Date(adate)
    print(adate)
    regkefs <- regtab[Date <= adate, list(intercept, slope)]
    if (withintercept) {
      tab[Date == adate, model := regkefs$intercept[nrow(regkefs)] + regkefs$slope[nrow(regkefs)]*newdiffs]
    } else {
   #   print('!!!!')
  #    print(regkefs$slope[nrow(regkefs)])
      tab[Date == adate, model := regkefs$slope[nrow(regkefs)]*newdiffs]
    }
    print(regkefs$slope[nrow(regkefs)])
    print(tail(tab[Date==adate, list(model, newdiffs)], 1))
  }
tab
}
FillRegModelInTabCurDate <- function(regtab, nextdate, critdate) {
  regs <- regtab[nrow(regtab),]
  mynewdiffs <- as.numeric(critdate) - as.numeric(nextdate)
  print(mynewdiffs*regs$slope[1] + regs$intercept[1])
  print(round(1000*(mynewdiffs*regs$slope[1] + regs$intercept[1])))
}
PlotChangeOfModels <- function(tab, restab, datestart) {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[Date >= datestart, ]
  restab <- restab[restab$Date >= datestart, ]
  plot(tab$Date, tab[, stavka/newdiffs])
  dates <- restab$Date[restab$changeflag]
  if (length(dates) > 0) {
    for (j in 1:length(dates)) {
      print(as.Date(dates[j]))
      abline(v = as.Date(dates[j]), col = "red")
    }
  }
}
CalculateRSqrd <- function(tab, kefs, withintercept) {
 # print("***")
  if (withintercept) {
    tab[, model := kefs[1] + kefs[2]*newdiffs]
  } else {
    tab[, model := kefs[1]*newdiffs]
  }
  tab[, 1-sum((model-stavka)^2)/sum((stavka - mean(stavka))^2)]
}
SplitDataForRegr <- function(tab) {
 # tab1 <- tab[synt<1181, ]
#  tab2 <- tab[1181<=synt & synt<2162, ]
#  tab3 <- tab[2162<=synt & synt<3812, ]
#  tab4 <- tab[3812<=synt & synt<4690, ]
#  tab5 <- tab[4690<=synt & synt<5737, ]
#  tab6 <- tab[synt >= 5737, ]
  tab1 <- tab[newdiffs < 19.5 & synt < 2290, ]
  tab2 <- tab[newdiffs>19.5 & newdiffs<71.5 & synt < 2290, ]
  tab3 <- tab[synt>=2290 & newdiffs<54.5, ]
  tab4 <- tab[synt>=2290 & newdiffs>54.5 & newdiffs<71.5, ]
  tab5 <- tab[newdiffs > 71.5 & newdiffs<99.5, ]
  tab6 <- tab[newdiffs>99.5, ]
list(tab1, tab2, tab3, tab4, tab5, tab6)
}
PrepareDailyData <- function(tab) {
  tab[, model := stavka - avstavka1expreal]
  aggtab1 <- tab[, mean(model), by = Date]
  aggtab2 <- tab[, head(model, 1), by = Date]
  aggtab3 <- tab[, quantile(model, 0.99), by = Date]
  aggtab4 <- tab[, quantile(model, 0.01), by = Date]
  aggtab5 <- tab[, mean(avstavka1), by = Date]
  aggtab6 <- tab[, head(newdiffs, 1), by = Date]
data.table(Date = aggtab1$Date, ModelMean = aggtab1$V1, ModelFirst = aggtab2$V1,
           ModelQuantPlus = aggtab3$V1, ModelQuantMinus = aggtab4$V1, ModStavka = aggtab5$V1,
           newdiffs = aggtab6$V1, diff = aggtab5$V1 - aggtab4$V1)
}
SlidingAutoArimaModel <- function(tab, datestart) {
  dates <- unique(tab[Date >= datestart, Date])
  res <- data.frame(Date = as.Date(numeric()), TrueVal = numeric(), ModVal = numeric())
  for (adate in dates) {
    print(as.Date(adate))
    vecred <- tab[Date < adate, ModStavka]
    vecdiffs <- diff(vecred)
    #arr <- auto.arima(vecdiffs)
    arr <- arima(vecdiffs, order = c(0, 0, 1))
    print(arr)
    valnext <- as.numeric(forecast(arr, 1)$mean) + vecred[length(vecred)]
    valcur <- tab[Date == adate, ModStavka]
    res <- rbind(res, data.frame(Date = as.Date(adate), TrueVal = valcur, ModVal = valnext))
  }
res
}

library(caret)
library(Hmisc)
library(xgboost)

FillxgBoostModStavka <- function(tab, datestart) {
  tabaggr <- tab[, list(mean(stavka), mean(newdiffs)), by = list(Date)]
  names(tabaggr) <- c("Date", "stavka", "newdiffs")
  tabaggr[, modstavka := stavka/newdiffs]
  tabaggr$valprev <- Lag(tabaggr$modstavka, 1)
  tabaggr$valprev2 <- Lag(tabaggr$modstavka, 2)
  tabaggr$valprev3 <- Lag(tabaggr$modstavka, 3)
  tabaggr$valprev4 <- Lag(tabaggr$modstavka, 4)
  tabaggr$valprev5 <- Lag(tabaggr$modstavka, 5)
  tabred <- na.omit(tabaggr[, list(Date, valprev, valprev2, valprev3,
                                   valprev4, valprev5, modstavka, newdiffs)])
  vec <- c()
  dates <- unique(tabred$Date)
  for (j in 1:length(dates)) {
    adate <- as.Date(dates[j])
    print(adate)
    if (adate >= datestart) {
      tabtrain <- tabred[Date < adate,]
     # print(tail(tabtrain))
      #mymodel <- xgboost(params = list(eta = 0.1, silent = 1,
      #                                 gamma = 0, min_child_weight = 0.1,
      #                                 subsample = 0.9, alpha = 0, lambda = 1,
      #                                 max_depth = 3, objective = "reg:linear",
      #                                 eval_metric = 'rmse'),
      #                   data = as.matrix(tabtrain[, 2:(ncol(tabred)-2), with = FALSE]),
      #                   label = tabtrain$modstavka, verbose = 0,
      #                   nrounds = 100)
      #mymodel <- xgboost(params = list(booster = 'gblinear', lambda = 10, alpha = 0),
      #                                 data = as.matrix(tabtrain[, 2:(ncol(tabred)-2), with = FALSE]),
      #                                 label = tabtrain$modstavka, verbose = 0, nrounds = 100)
      mymodel <- avNNet(modstavka ~ valprev+valprev2+valprev3+valprev4+valprev5, data = tabred, repeats = 100, size = 10, trace = FALSE, linout = TRUE)
      tabtest <- tabred[Date == adate, ]
   #   print( as.matrix(tabtrain[, 2:(ncol(tabred)-2), with = FALSE]))
   #   print(tabtrain$modstavka)
      temp <- predict(mymodel, as.matrix(tabtest[, 2:(ncol(tabtest)-2), with = FALSE]))
      vec <- c(vec, temp)
    }
  }
data.frame(Date = tabred[Date >= datestart, Date],
           val = tabred[Date >= datestart, modstavka*newdiffs],
           model = vec*tabred[Date >= datestart, newdiffs])

}

FillxgBoost <- function(tab, datestart) {
  tabaggr <- tab[, list(mean(stavka), mean(newdiffs)), by = list(Date)]
  names(tabaggr) <- c("Date", "val", "newdiffs")
  tabaggr$valprev <- Lag(tabaggr$val, 1)
  tabaggr$valprev2 <- Lag(tabaggr$val, 2)
  tabaggr$valprev3 <- Lag(tabaggr$val, 3)
  tabaggr$valprev4 <- Lag(tabaggr$val, 4)
  tabaggr$valprev5 <- Lag(tabaggr$val, 5)
  tabaggr$newdiffsprev <- Lag(tabaggr$newdiffs, 1)
  tabaggr$newdiffsprev2 <- Lag(tabaggr$newdiffs, 2)
  tabaggr$newdiffsprev3 <- Lag(tabaggr$newdiffs, 3)
  tabaggr$newdiffsprev4 <- Lag(tabaggr$newdiffs, 4)
  tabaggr$newdiffsprev5 <- Lag(tabaggr$newdiffs, 5)
  tabaggr$newvalprev <- tabaggr[, (valprev/newdiffsprev)*newdiffs]
  tabaggr$newvalprev2 <- tabaggr[, (valprev2/newdiffsprev2)*newdiffs]
  tabaggr$newvalprev3 <- tabaggr[, (valprev3/newdiffsprev3)*newdiffs]
  tabaggr$newvalprev4 <- tabaggr[, (valprev4/newdiffsprev4)*newdiffs]
  tabaggr$newvalprev5 <- tabaggr[, (valprev5/newdiffsprev5)*newdiffs]
  tabred <- na.omit(tabaggr[, list(Date, valprev, valprev2, valprev3, valprev4, valprev5,
                           newvalprev, newvalprev2, newvalprev3, newvalprev4, newvalprev5,
                           val)])
  vec <- c()
  dates <- unique(tabred$Date)
  for (j in 1:length(dates)) {
    adate <- as.Date(dates[j])
    print(adate)
    print(j)
    if (adate >= datestart) {
      tabtrain <- tabred[Date < adate,]
      mymodel <- xgboost(params = list(silent = 1, eta = 0.01, gamma = 1,
                                       min_child_weight = 1, subsample = 0.65,
                                       max_depth = 3, objective = "reg:linear"),
                         data = as.matrix(tabtrain[, 2:(ncol(tabred)-1), with = FALSE]),
                         label = tabtrain$val, verbose = 0,
                         nrounds = 30)
      tabtest <- tabred[Date == adate, ]
      temp <- predict(mymodel, as.matrix(tabtest[, 2:(ncol(tabtest)-1), with = FALSE]))
      print(temp)
  #    print(tabtest$val)
  #    print(tabtest$val-temp)
      vec <- c(vec, temp)
    }
  }
data.frame(Date = tabred[Date >= datestart, Date],
           val = tabred[Date >= datestart, val],
           model = vec)
}

FillNeuralNet <- function(tab, datestart) {
  tabaggr <- tab[, list(mean(stavka), mean(newdiffs)), by = list(Date)]
  names(tabaggr) <- c("Date", "val", "newdiffs")
  tabaggr$valprev <- Lag(tabaggr$val, 1)
  tabaggr$valprev2 <- Lag(tabaggr$val, 2)
  tabaggr$valprev3 <- Lag(tabaggr$val, 3)
  tabaggr$valprev4 <- Lag(tabaggr$val, 4)
  tabaggr$valprev5 <- Lag(tabaggr$val, 5)
  tabaggr$newdiffsprev <- Lag(tabaggr$newdiffs, 1)
  tabaggr$newdiffsprev2 <- Lag(tabaggr$newdiffs, 2)
  tabaggr$newdiffsprev3 <- Lag(tabaggr$newdiffs, 3)
  tabaggr$newdiffsprev4 <- Lag(tabaggr$newdiffs, 4)
  tabaggr$newdiffsprev5 <- Lag(tabaggr$newdiffs, 5)
#  tabaggr$V4 <- c(NA, NA, tabaggr$V1[1:(nrow(tabaggr)-2)])
#  tabaggr$V5 <- c(NA, NA, NA, tabaggr$V1[1:(nrow(tabaggr)-3)])
  tabaggr$model1 <- tabaggr[, (valprev/newdiffsprev)*newdiffs]
  tabaggr$model3 <- tabaggr[, (valprev/newdiffsprev + valprev2/newdiffsprev2 + valprev3/newdiffsprev3)*newdiffs/3]
  tabaggr[, model5 := (valprev/newdiffsprev + valprev2/newdiffsprev2 + valprev3/newdiffsprev3 + valprev4/newdiffsprev4 + valprev5/newdiffsprev5)*newdiffs/5]
  tabaggr <- na.omit(tabaggr)
  dates <- tabaggr$Date
  tabaggr$model <- NA
  for (j in 1:length(dates)) {
    adate <- as.Date(dates[j])
    print(adate)
    print(j)
    if (adate >= datestart) {
      tabtrain <- tabaggr[Date < adate,]
      #mymodel <- avNNet(val ~ I((valprev/newdiffsprev)*newdiffs) + I((valprev2/newdiffsprev2)*newdiffs) +
      #                    I((valprev3/newdiffsprev3)*newdiffs) + I((valprev4/newdiffsprev4)*newdiffs) +
      #                    I((valprev5/newdiffsprev5)*newdiffs),
      #                  data = tabtrain, repeats = 100, size = 10, trace = FALSE, linout = TRUE)
      #mymodel <- avNNet(val ~ model5, data = tabtrain, repeats = 100, size = 10, trace = FALSE, linout = TRUE)
      #mymodel <- avNNet(val ~ model3 + valprev, data = tabtrain, repeats = 100, size = 10, trace = FALSE, linout = TRUE)
      mymodel <- avNNet(val ~ model3, data = tabtrain, repeats = 100, size = 10, trace = FALSE, linout = TRUE)
   #   print(predict(mymodel))
  #    print(predict(mymodel, newdata = tabaggr[Date <= adate, ]))
      temp <- predict(mymodel, newdata = tabaggr[Date <= adate, ])
      tabaggr$model[j] <- temp[length(temp)]
    }
  }
tabaggr
}
AdaptiveRegressionCallFast <- function(datestart, dateend, curfutname, critdate, nextdate, nextnextdate, control = 1) {
  tab <- ResearchDataFast(datestart, dateend, curfutname = curfutname, critdate = critdate,
                          nextdate = nextdate, n = 10)
  regtab <- FillAdaptiveRegression(tab, 7, 0.5, withintercept = FALSE)
  if (control == 1) {
    return(FillRegModelInTabCurDate(regtab, nextnextdate, critdate))
  } else {
    return(MakeShortTab(FillRegModelInTab(tab, regtab, datestart, withintercept = FALSE), aname = "model"))
  }
}
