library(TTR)
library(caTools)
#requires SimulatorScrips.R, DollarStavkaGraphs.R, Research.R
PrepareDataForSimulation <- function(datestart, dateend, curfutname, num, fileind) {
  tab <- MainData(datestart, dateend, curfutname, "1mVol50", num)
  tab <- DecreaseBySwaps(tab, tab$newmodel)
  ktab <- aggregate(tab$truestavka, list(tab$Date), function(x) {x[1]})
  WriteZeroPointsForArbitrazhor(ktab, id = fileind, mod = -1000, pricestep = 2.5)
ktab
}
PrepareStandardModel <- function(datestart, dateend, curfutname, num, nextdate, critdate) {
  tab <- ResearchDataFast(datestart, dateend, curfutname = "SIZ6@FORTS",
                          critdate = critdate, nextdate = nextdate, n = num)
  tab[, newmodel := avstavka1expreal]
  tab[, newstavka := stavka]
na.omit(tab)
}
PrepareStandardModelDataForSimulation <- function(datestart, datestarteff, dateend, curfutname, num, nextdate, critdate, rate, filename, standard = TRUE, num2 = NA) {
  if (is.na(num2)) {num2 <- num}
  if (standard) {
    tab <- PrepareStandardModel(datestart, dateend, curfutname, num*748, nextdate, critdate)
  } else {
    tab <- MainData(datestart, dateend, curfutname, "1mVol50", num)
    tab <- DecreaseBySwaps(tab, tab$newmodel)
    tab$newstavka <- tab$newstavka - tab$Val
    tab$newmodel <- tab$truestavka
  }
 # return(tab)
  tab2 <- GetSpreadInfoByTwoQuantiles(tab, num2, rate, 0.99, datestarteff)
  shifts <- tab2$stepstotake
  names(shifts) <- c("Date", "Val")
  shifts$Val <- 1000/shifts$Val
  tab <- tab[Date >= datestarteff, ]
  zerotab <- tab[, head(newmodel, 1), by = list(Date)]
  spreadstab <- tab2$spreads
  names(spreadstab) <- c("Date", "Spreads")
  spreadstab <- spreadstab[Date >= datestarteff, ]
  spreadstab$Spreads <- 2*spreadstab$Spreads
  print(summary(spreadstab$Spreads))
#  stop()
  names(zerotab) <- c("Date", "Val")
  hdat <- cbind(zerotab, spreadstab$Spreads, data.frame(VV = rep(1, nrow(zerotab))), shifts$Val)
  #return(hdat)
  hdat$Val <- round(-1000*hdat$Val/2.5)
  hdat$Date <- as.character(format(hdat$Date, format = "%d.%m.%Y"))
  hdat$V4 <- as.character(paste0(as.character(round(hdat$V4))))
  write.table(hdat, paste0("restab", filename, ".csv"), col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE, append = FALSE)
hdat
}
#AddStandardModelDeviations <- function(tab, datestart, rate, num) {
#  tab2 <- AddModelDeviations(tab, num, rate, FALSE)
#  tab2 <- tab2[Date >= datestart, ]
#}
GetAverageSwaps <- function(datestart, dateend, timestart, timeend,
                            candle.type = "1sVol50", storage.path = "D:\\") {
  tabswaps <- na.omit(LoadBp("USD000TODTOM@CETS", datestart, dateend,
                    candle.type = candle.type, start.time = timestart, end.time = timeend,
                    storage.path = storage.path))
  tabswapsgr <- tabswaps[, mean(Close), by = list(Date)]
  tabswapsgr2 <- tabswaps[, mean(Ask-Bid), by = list(Date)]
  tabswapsgr <- data.table(tabswapsgr$Date, tabswapsgr$V1,
                           tabswapsgr2$V1)
  names(tabswapsgr) <- c("Date", "Val", "Spread")
  tabswapsgr[, NumOfSwaps := round(Val/median(Val))]
tabswapsgr
}
GetAverageTodToms <- function(datestart, dateend, timestart, timeend) {
  tabtom <- na.omit(LoadBp("USD000UTSTOM@CETS", datestart, dateend,
                           candle.type = "1sVol50", start.time = timestart, end.time = timeend,
                           storage.path = "D:\\"))
  tabtod <- na.omit(LoadBp("USD000000TOD@CETS", datestart, dateend,
                           candle.type = "1sVol50", start.time = timestart, end.time = timeend,
                           storage.path = "D:\\"))
  tabswaps <- InnerMergeDf(list(tom = tabtom, tod = tabtod), "Time")
  tabswapsgr1 <- tabswaps[, mean(tom.Close - tod.Close), by = list(tom.Date)]
  tabswapsgr2 <- tabswaps[, mean(tom.Ask-tod.Bid), by = list(tom.Date)]
  tabswapsgr <- data.table(data.frame(Date = tabswapsgr1[, 1, with = FALSE],
                         Val = tabswapsgr1[, 2, with = FALSE],
                         Spread = tabswapsgr2[, 2, with = FALSE]))
  names(tabswapsgr) <- c("Date", "Val", "Spread")
  tabswapsgr[, NumOfSwaps := round(Val/median(Val))]
tabswapsgr
}
DecreaseBySwaps <- function(gtab, aval) {
  gtab[, truestavka := aval - Val]
}
IncreaseStavkaBySwaps <- function(tab, tabswaps) {
  tab <- as.data.table(as.data.frame(tab))
  tabswaps <- as.data.table(as.data.frame(tabswaps))
  dates <- unique(tab$Date)
  extradates <- dates[!(dates %in% tabswaps$Date)]
  print(extradates)
  if (length(extradates)>0) {
    tabswaps <- rbind(tabswaps,
                      data.table(Date = extradates,
                                 Val = rep(0, length(extradates)),
                                 Spread = rep(0, length(extradates)),
                                 NumOfSwaps = rep(0, length(extradates))
                                )
                      )
  }
  setkey(tabswaps, "Date")
  tabswaps[, Val := cumsum(Val)]
  setkey(tab, "Date")
  gtab <- tab[tabswaps]
  setkey(gtab, "Time")
  gtab[, newstavkabid := stavkabid + Val]
  gtab[, newstavkaask := stavkaask + Val]
  gtab[, newstavka := stavka + Val]
gtab
}
GetMeanTab <- function(tab) {
  tab[, mean(newstavka), by = list(Date)]
}
AddSmaModel <- function(tab, num) {
  tab[, newmodel := SMA(tab[, newstavka], num)]
}
AddEmaModel <- function(tab, num) {
  tab[, newmodel := EMA(tab[, newstavka], num)]
}
AddModelDeviations <- function(tab, num, rate, secmode = FALSE) {
  tab <- as.data.table(as.data.frame(tab))
  dates <- unique(tab$Date)
  tab <- as.data.table(as.data.frame(tab))
  tab[, devs := newmodel - newstavka]
  if (secmode) {
    tabred <- tab[seq(1, nrow(tab), 60), ]
  } else {
    tabred <- tab
  }
  num <- num*748
  ###this is a 3-days forwar window! not a backward one
  myquant <- runquantile(abs(tabred$devs), num, rate, endrule = "NA", align = "right")
#  myquant <- c(rep(NA, nrow(tab) - length(myquant)), myquant)
  tabred[, quant := myquant]
  firstval <- function(x) {x[1]}
  firsttab <- tabred[, firstval(quant), by = list(Date)]
  names(firsttab) <- c("Date", "firstquant")
  setkey(tab, "Date")
  setkey(firsttab, "Date")
  ctab <- tab[firsttab]
  setkey(ctab, "Time")
  ctab
}
CalculateAnnStavka <- function(vec, datestart, critdate, dolval) {
  vec*100*365/(dolval*(as.numeric(critdate) - as.numeric(datestart)))
}
PlotWithDeviations <- function(tab, datestart, dateend, emulatestavka = FALSE, critdate = as.Date("2016-12-16"), dolval = 66) {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[Date >= datestart & Date <= dateend, ]
  tab[, id := 1:nrow(tab)]
  print(head(tab))
  if (emulatestavka) {
    tab[, newstavka := CalculateAnnStavka(newstavka, datestart, critdate, dolval)]
    tab[, firstquant := CalculateAnnStavka(firstquant, datestart, critdate, dolval)]
    tab[, newmodel := CalculateAnnStavka(newmodel, datestart, critdate, dolval)]
  }
  firstfun <- function(x) {x[1]}
  datetab <- as.data.frame(tab[, firstfun(id), by = list(Date)])
  plot(tab$newstavka, main = paste("Stavka till", max(tab$Date)), ylim = c(1.55, 1.75))
  lines(tab$newmodel, col = "red")
  lines(tab$newmodel + tab$firstquant, col = "green")
  lines(tab$newmodel - tab$firstquant, col = "green")
  for (j in 1:nrow(datetab)) {
    abline(v = datetab[j, 2], col = "blue")
  }
}
EstimateArbitrageCharactertistics <- function(tab, datestart, dateend, ratio, eps, secmode = FALSE) {
  tab <- as.data.table(as.data.frame(tab))
  if (!secmode) {
    tab <- AddModelDeviations(tab, 3*745, ratio, secmode = secmode)
  } else {
    tab <- AddModelDeviations(tab, 3*745, ratio, secmode = secmode)
  }
  tab[, firstquant := firstquant + eps]
  tab[, devdiff := devs-firstquant]
  tab[, devdiff := ifelse(devdiff>0, 1, 0)]
  tab[, delta := c(0, diff(devdiff))]
  indsplus <- which(tab$delta > 0)
  indsminus <- which(tab$delta < 0)
  if (indsminus[1] < indsplus[1]) {indsminus <- indsminus[2:length(indsminus)]}
  newinfo <- list(plus = indsplus, minus = indsminus)
data.frame(ratio = ratio, eps = eps, len = mean(indsminus[1:min(length(indsplus), length(indsminus))]-indsplus[1:min(length(indsplus), length(indsminus))]), tot = length(indsplus))
}
MainEst <- function(tab, datestart, dateend, secmode) {
  res <- data.frame(ratio = numeric(), eps = numeric(), len = numeric(), tot = numeric())
  for (ratio in seq(0.6, 1, 0.01)) {
    print(ratio)
    res <- rbind(res, EstimateArbitrageCharactertistics(tab, datestart, dateend,
                                                        ratio, 0, secmode = secmode))
  }
  res
}
MainData <- function(datestart, dateend, curfutname, candletype, num = 3) {
  tab <- FillStavkaTab(datestart, dateend, futsec = curfutname, candletype = candletype,
                       storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\")
  tabswaps <- GetAverageSwaps(datestart, dateend, "10:10:00", "10:15:00")
  print(tabswaps)
  gtab <- na.omit(IncreaseStavkaBySwaps(tab, tabswaps))
  htab <- gtab[, length(dol.bid), by = list(Date)]
  print(htab)
  htab <- htab[V1>500, ]
  gtab <- gtab[Date %in% unique(htab$Date), ]

  if (grepl("1s", candletype)) {
    num <- num*44703
  } else {
    num <- num*748
  }
  gtab <- na.omit(AddEmaModel(gtab, num = num))
  htab <- gtab[, length(dol.bid), by = list(Date)]
  print(htab)
  htab <- htab[V1>500, ]
  gtab <- gtab[Date %in% unique(htab$Date), ]

  gtab
}
MainTester <- function(datestart, dateend, curfutname, datestarteff) {
  for (num1 in 1:5) {
    tab <- MainData(datestart, dateend, curfutname, "1mVol50", num1)
    for (num2 in 1:5) {
      for (rate in seq(0.6, 1, 0.01)) {
 #       print(tail(tab))
        tab2 <- AddModelDeviations(tab, num2, rate, FALSE)
        tab3 <- na.omit(tab2[Date >= datestart, ])
#        print(tail(tab3))
        for (logval in c(TRUE, FALSE)) {
          res <- strategy_DSSD(as.data.table(as.data.frame(tab3)), data.frame(maxpos = 20, closebymean = logval))
          write.table(data.frame(
                       num1, num2, rate, logval,
                       res$pl, res$numofdeals, res$curpos, res$numofdays),
                      "varspreads2.csv", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ";")
#stop()
        }
      }
    }
  }
}
GetSpreadInfoByTwoQuantiles <- function(tab, num, rate1, rate2, datestart, asset = "spot") {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[Date >= datestart, ]
  tab1 <- na.omit(AddModelDeviations(tab, num, rate1, FALSE))
  tab2 <- na.omit(AddModelDeviations(tab, num, rate2, FALSE))
  tab1[, firstquant2 := tab2$firstquant]
  if (asset == "spot") {
    multip <- 2.5
  } else {
    multip <- 1
  }
  tab1[, myspread := round(firstquant*1000/multip)]
  tab1[, stepstotake := round(firstquant2*1000/multip) - myspread]
  agtab1 <- tab1[, mean(myspread), by = Date]
  agtab2 <- tab1[, mean(stepstotake), by = Date]
  agtab3 <- tab1[, round(1000*quantile(abs(devs), rate2)/multip), by = Date]
list(spreads = agtab1, stepstotake = agtab2, realquantiles = agtab3)
}
PrepareDataForGarch <- function(tab) {
  tab <- na.omit(as.data.table(as.data.frame(tab)))
  tab[, timehour := fastPOSIXct(paste0(Date, " ", hour(Time), ":00:00"))]
  restab <- tab[, lapply(.SD, head, n = 1), by = timehour, .SDcols = c("newmodel", "newstavka")]
  restab[, eps := newstavka - newmodel]
restab
}
PrepareFirstQuant <- function(tab, datestart) {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[Date >= datestart, ]
tab[, mean(quant), list(Date)]
}
GetAllStavData <- function() {
  tabdec <- MainData(as.Date("2016-09-01"), as.Date("2016-11-08"), "SIZ6@FORTS", "1mVol50", 1)
  tabdec <- tabdec[Date >= as.Date("2016-09-15"), ]
  tabsept <- MainData(as.Date("2016-06-01"), as.Date("2016-09-14"), "SIU6@FORTS", "1mVol50", 1)
  tabsept <- tabsept[Date >= as.Date("2016-06-15"), ]
  tabjune <- MainData(as.Date("2016-03-01"), as.Date("2016-06-14"), "SIM6@FORTS", "1mVol50", 1)
  tabjune <- tabjune[Date >= as.Date("2016-03-15"), ]
  tabmarch <- MainData(as.Date("2015-12-01"), as.Date("2016-03-14"), "SIH6@FORTS", "1mVol50", 1)
  tabmarch <- tabmarch[Date >= as.Date("2015-12-15"), ]
  tabdecprev <- MainData(as.Date("2015-09-01"), as.Date("2015-12-14"), "SIZ5@FORTS", "1mVol50", 1)
  tabdecprev <- tabdecprev[Date >= as.Date("2015-09-15"), ]
rbind(tabdecprev, tabmarch, tabjune, tabsept, tabdec)
}
FillVolShifts <- function(quanttab, spread, bank) {
  quanttab <- as.data.table(as.data.frame(quanttab))
  quanttab[, Val := round(pmax(Val, spread))]
  quanttab[, numofhits := Val - spread + 1]
  quanttab[, myspread := spread]
  quanttab[, ShiftVolume := bank/numofhits]
  quanttab[, ShiftLot := 1]
quanttab
}
GetModelAndQuantilesInfo <- function(datestart, dateend, critdate, nextdate, n, datestarteff, asset = "spot", modelnum = 1,
                                     researchmode = FALSE, N = 8, curfutname = "SIH7@FORTS") {
  if (modelnum == 1) {
    tab <- na.omit(ResearchDataFast(datestart, dateend, curfutname = curfutname,
                          critdate = critdate, nextdate = nextdate, n = n))
    tab[, newstavka := stavka]
    tab[, newmodel := avstavka1expreal]
  } else {
    tab <- MainData(datestart, dateend, curfutname, "Data", num = n/750)
    tab <- DecreaseBySwaps(tab, tab$newmodel)
    tab[, newmodel := truestavka]
    tab[, newstavka := stavka]
  }
  if (asset == "spot") {
    spread0 <- 15
  } else {
    spread0 <- round(2.5*15)
  }
  qinfo <- GetSpreadInfoByTwoQuantiles(tab, n/750, 0.68, 0.997, datestart, asset = asset)
  qtab <- data.frame(Date = qinfo$spreads$Date, Val = qinfo$spreads$V1 + qinfo$stepstotake$V1)
  if (researchmode) {
    #return(data.frame(Date = qinfo$realquantiles$Date, Val = qinfo$realquantiles$V1))
    return(qinfo$realquantiles)
  }
  qtabreal <- data.frame(Date = qinfo$realquantiles$Date, Val = pmax(spread0, qinfo$realquantiles$V1))
  vec <- SimplePredictTrueQuantiles(qinfo$realquant, N)
  qtabpredict <- data.frame(Date = qinfo$realquantiles$Date,
                            Val = c(NA, vec[1:(length(vec)-1)]))
  qtabpredict <- qtabpredict[qtabpredict$Date >= datestarteff, ]
  qtab <- qtab[qtab[, 1] >= datestarteff, ]
  qtabreal <- qtabreal[qtabreal[, 1] >= datestarteff, ]
  print(head(tab))
  ktab <- as.data.frame(MakeShortTab(tab, aname = "newmodel"))
  ktab <- ktab[ktab[, 1] >= datestarteff, ]
  qtab <- FillVolShifts(qtab, spread0, 1000)
  qtabreal <- FillVolShifts(qtabreal, spread0, 1000)
  qtabpredict <- FillVolShifts(qtabpredict, spread0, 1000)
  list(ktab = ktab, qtab = qtab, qtabreal = qtabreal, qtabpredict = qtabpredict)
}
ChooseNToPredictTrueQuantiles <- function(qtabpure, myn = NA, spread0 = NA,
                                          datestarteff = as.Date("2016-09-15")) {
  vec <- qtabpure$V1
  stattab <- data.frame(n = 1:15, Val = numeric(15))
  for (n in 1:15) {
    qtabcycled <- as.data.table(as.data.frame(qtabpure))
    predictor <- EMA(vec, n = n)
    qtabcycled[, predictor := c(NA, predictor[1:length(predictor)-1])]
    qtabcycled <- qtabcycled[Date >= datestarteff, ]
    if (!is.na(spread0)) {
      qtabcycled[V1 < spread0, V1 := spread0]
      qtabcycled[predictor < spread0, predictor := spread0]
      qtabcycled <- qtabcycled[V1>spread0, ]
    }
    print(qtabcycled)
    stattab$Val[n] <- sum((qtabcycled$predictor - qtabcycled$V1)^2)
    if (!is.na(myn)) {
      if (n == myn) {
        return(qtabcycled)
      }
    }
  }
stattab
}
SimplePredictTrueQuantiles <- function(qtabpure, n) {
  qtabpure <- as.data.table(as.data.frame(qtabpure))
  qtabpure$predictor <- EMA(qtabpure$V1, n)
  qtabpure$predictor
}
FillRollingRegression <- function(quanttab, datestarteff) {
   quanttab <- as.data.table(as.data.frame(quanttab))
   quanttab$model <- NA
   quanttabtemp <- quanttab[Date < datestarteff, ]
   regres <- as.numeric(coef(lqs(quanttabtemp$V1 ~ quanttabtemp$newdiffs)))
   quanttab <- quanttab[Date >= datestarteff, ]
   for (j in 1:nrow(quanttab)) {
     quanttab$model[j] <- regres[1] + regres[2]*quanttab$newdiffs[j]
     quanttabtemp <- rbind(quanttabtemp, quanttab[j, ])
     regres <- as.numeric(coef(lqs(quanttabtemp$V1 ~ quanttabtemp$newdiffs)))
   }
quanttab
}
ChooseRollingResiduals <- function(quanttab) {
  vec <- quanttab$resids
  restab <- data.frame(n = 1:10, res = numeric(10))
  for (n in 1:10) {
    emavec <- EMA(vec, n = n)
    emavec <- c(NA, emavec[1:(length(emavec)-1)])
    mystat <- sum(abs(quanttab$resids - emavec), na.rm = TRUE)
    restab$res[n] <- mystat
  }
restab
}
FillResidsModel <- function(quanttab) {
  vec <- quanttab$resids
  emavec <- EMA(vec, n = 4)
  quanttab$emavec <- c(NA, emavec[1:(length(emavec)-1)])
quanttab
}
GetFullModel <- function(quanttab, datestarteff) {
  quanttab <- as.data.table(as.data.frame(quanttab))
  quanttab <- FillRollingRegression(quanttab, as.Date(datestarteff-6))
  quanttab[, resids := V1 - model]
  quanttab <- FillResidsModel(quanttab)
  quanttab[, mymodel := model + emavec]
  quanttab[, mymodel := mymodel + 15]
  quanttab[, V1 := V1 + 15]
  quanttab[, mymodel := pmax(mymodel, 37)]
na.omit(quanttab)
}
GetDiffsTab <- function(datestart, dateend, critdate, nextdate, curfutname, n) {
  quanttab <- GetModelAndQuantilesInfo(datestart, dateend, critdate, nextdate, n, datestart, asset = "fut", modelnum = 1,
                                       researchmode = TRUE, N = NA, curfutname = curfutname)
  tab <- ResearchDataFast(datestart, dateend,
                          curfutname = curfutname, critdate = critdate, nextdate = nextdate, n = n)
  tabdiffs <- aggregate(tab$newdiffs, list(tab$Date), mean)
  names(tabdiffs) <- c("Date", "newdiffs")
  tabdiffs <- tabdiffs[tabdiffs$Date %in% quanttab$Date, ]
  quanttab <- quanttab[quanttab$Date %in% tabdiffs$Date, ]
  quanttab$newdiffs <- tabdiffs$newdiffs
quanttab
}
PlotQuantTab <- function(quanttab) {
  plot(quanttab$newdiffs, quanttab$V1)
  points(quanttab$newdiffs, quanttab$mymodel, col = "red")
}
