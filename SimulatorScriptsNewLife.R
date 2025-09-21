#requires SimulatorScripts.R
ChooseSwapMoment <- function(report, datestart, dateend, curswapname, curspotname, nextday, rough = TRUE, curfutname = NA) {
  print(datestart)
  print(dateend)
  mytimes <- seq(as.POSIXct(paste(origin, "10:01:00")), as.POSIXct(paste(origin, "15:30:00")), 60)
  mytimes <- strftime(mytimes, "%H:%M:%S")
  if (rough) {
    res <- data.frame(time = mytimes, profs = NA)
  }
  tabquotes = NA
  for (j in 1:length(mytimes)) {
    atime <- mytimes[j]
    if (rough) {
      rep <- MainCalculateRepos(report, datestart, dateend, curswapname,
                              curspotname, atime, nextday)
      #stop()
      prof <- GetPlFromOptimFile(report)
      res$profs[j] <- prof[`ID прогона` == 6, TruePl]
      #break()
    } else {
      print(atime)
      restab <- MainCalculatePlForReport(report, curfutname, tabquotes = tabquotes, tabswaps = NA, atime = atime)
      if (is.na(tabquotes)) {
        tabquotes <- restab$tabquotes
        res <- MainAnalyzeSimResults(restab$res, 12000000, TRUE)
      } else {
        res <- rbind(as.data.frame(res), as.data.frame(MainAnalyzeSimResults(restab$res, 12000000, TRUE)))
      }
    }
  }
#na.omit(cbind(mytimes, res))
  res$mytimes <- mytimes
res
}
GetFullRelevantInfo <- function(report, num, curfutname) {
  if (num<10) {
    num <- paste0("0000", num)
  } else {
    num <- paste0("000", num)
  }
  report <- paste0(report, "IterationsResult\\", num, "\\")
  dat <- MainCalculatePlForReport(report, curfutname)
  MainAnalyzeSimResults(dat, 12000000)
}
GetPlFromOptimFile <- function(report) {
  opres <- fread(paste(report, "optimizerResult.csv", sep = "\\"), dec = ",", sep = ";")
  opres[, AvgPL := as.numeric(gsub(",", ".", AvgPL))]
  opres[, Commission := as.numeric(gsub(",", ".", Commission))]
  opres[, TruePl := swappl + AvgPL - Commission]
  opres <- opres[order(opres$TruePl, decreasing = TRUE), ]
opres
}
MainAnalyzeSimResults <- function(restab, bank, nopicture = FALSE) {
  datelen <- as.numeric(restab$Date[nrow(restab)]) - as.numeric(restab$Date[1]) + 1
  pl <- restab$pl[nrow(restab)]
  pldiffs <- c(restab$pl[1], diff(restab$pl))
  print(c(nrow(restab), length(pldiffs)))
  medpl <- median(pldiffs)*nrow(restab)
  annpl <- 100*365*pl/(datelen*bank)
  medannpl <- 100*250*median(pldiffs)/bank
  mysharp <- pl/(nrow(restab)*sd(pldiffs))
 # sharp <- (pl/nrow(restab) - 0.1*bank/365)/(sd(pldiffs))
  logpl <- c(bank, bank + restab$pl)
#  print(head(logpl))
  logpldiff <- logpl[2:length(logpl)]/logpl[1:(length(logpl) - 1)]
#  print(head(logpldiff))
#  print(mean())
  sharp <- mean(logpldiff-1)/sd(logpldiff-1)
  sortino <- pl/(nrow(restab)*sd(pldiffs[pldiffs<0]))
  maxpos <- restab$maxpos[nrow(restab)]
  dealsdiffs <- c(restab$numlots[1], diff(restab$numlots))
  numdays <- length(dealsdiffs[dealsdiffs>0])/length(dealsdiffs)
  meandeals <- mean(dealsdiffs)
  slipval <- tail(restab$slipval, 1)
  #plot(restab$Date, restab$pl*100*365/(bank*(as.numeric(restab$Date)-as.numeric(restab$Date[1]))))
  restabb <- data.frame(Date = c(restab$Date[1]-1,restab$Date),
                        pl = c(0, restab$pl))
  if (!nopicture) {
    plot(restabb$Date, restabb$pl, xlab = "Дата", ylab = "Прибыль", type = "l")
    #10 = 100*365*xxx/(datelen*bank)
    #xxx = 10*datelen*bank/(100*365)
    #0 = kef*date1 + b
    #xxx = kef*date2 + b
    #
    #  abline(coef=c(0, 10*bank*datelen/(100*365)), col = "red")
    segments(restabb$Date[1], 0, restabb$Date[nrow(restabb)], 10*bank*datelen/(100*365), col = "red")
  }
  list(pl = pl, medpl = medpl, annpl = annpl, medannpl = medannpl,
       mysharp = mysharp,
       sharp = sharp,
       sortino = sortino,
       maxpos = maxpos, numdays = numdays,
       meandeals = meandeals,
       slipval = slipval)
}
MainBackTestCalculator <- function(basereport, curfutname, atime = "10:20:00") {
  optimres <- fread(paste(basereport, "optimizerResult.csv", sep = "\\"))
  idlist <- optimres$`ID прогона`
  restab <- data.frame(id = idlist,
                       annpl = numeric(length(idlist)),
                       medannpl = numeric(length(idlist)),
                       sharp = numeric(length(idlist)),
                       sortino = numeric(length(idlist)),
                       numdays = numeric(length(idlist)))
  count <- 1
  for (id in idlist) {
    print(id)
    report <- paste0(basereport, "\\IterationsResult\\", DefineFileById(id, ""))
    if (count == 1) {
      res <- MainCalculatePlForReport(report, curfutname, tabquotes = NA, tabswaps = NA, atime = atime)
      repcalc <- res$res
      tabquotes <- res$tabquotes
      tabswaps <- res$tabswaps
    } else {
      res <- MainCalculatePlForReport(report, curfutname, tabquotes = tabquotes, tabswaps = tabswaps, atime = atime)
      repcalc <- res$res
    }
    repres <- MainAnalyzeSimResults(repcalc, 12000000, TRUE)
    restab$annpl[id] <- repres$annpl
    restab$medannpl[id] <- repres$medannpl
    restab$sharp[id] <- repres$sharp
    restab$sortino[id] <- repres$sortino
    restab$numdays[id] <- repres$numdays
    count <- count +1
  }
restab
}
PosStat <- function(x) {
  v1 <- max(x)
  v2 <- abs(min(x))
  if (v1>=v2) {return(v1)} else {return(-v2)}
}
MainCalculatePlForReport <- function(report, curfutname, tabquotes = NA, tabswaps = NA, atime = "10:20:00") {
#  print(paste(report, "trades.csv", sep = ""))
  deals <- fread(paste(report, "trades.csv", sep = ""))
  curves <- fread(paste(report, "curves.csv", sep = ""))
  dates <- unique(as.Date(curves$Time))
  deals[, Date := as.Date(Time)]
  dealsspot <- deals[Security == "USD000UTSTOM@CETS", ]
  dealsfut <- deals[Security == curfutname, ]
  dealsspot[, Pos := cumsum(ifelse(Direction == "Buy", Volume, -Volume))]
  dealsfut[, Pos := cumsum(ifelse(Direction == "Buy", Volume, - Volume))]
  extrafuttab <- dealsfut[, list(PosStat(Pos), sum(abs(Volume))), by = Date]

  dealsfut2 <- dealsfut
  #return(dealsfut2[, max(abs(Pos)), by = Date])

#  print(head(dealsspot))
#  print(head(dealsfut))
  datestart <- dates[1]
  dateend <- dates[length(dates)]
  if (is.na(tabquotes)) {
    tabquotes <- GetQuotesTab(datestart, dateend, "23:40:00", curfutname, candletype = "Data")
  }
  if (is.na(tabswaps)) {
    tabswaps <- GetSwapData(datestart, dateend, atime, "Data")
  }
  swaptab <- AddSwaps(dates, dealsspot, tabswaps)
  dates <- unique(swaptab$Date)
  datestart <- dates[1]
#  plprev <- 0
#  pureplprev <- 0
#  pureplnoswapsprev <- 0
  restab <- data.frame(Date = rep(Sys.Date(), length(dates)),
                       pl = numeric(length(dates)),
                       purepl = numeric(length(dates)),
                       pureplnoswaps = numeric(length(dates)),
                       numlots = numeric(length(dates)),
                       maxpos = numeric(length(dates)),
                       slipval = numeric(length(dates)),
                       curpos = numeric(length(dates)))
  for (j in 1:length(dates)) {
    dateend <- dates[j]
 #   print(dateend)
    plspot <- PlCalc(dealsspot[Date <= dateend, ], tabquotes$dol[Date == dateend, ], 1000)
    plfut <- PlCalc(dealsfut[Date <= dateend, ], tabquotes$fut[Date == dateend, ], 1)
    pl <- plspot$pl + plfut$pl + swaptab$SwapVal[swaptab$Date == dateend]
    purepl <- plspot$purepl + plfut$purepl + swaptab$SwapVal[swaptab$Date == dateend]
    pureplnoswaps <- plspot$purepl + plfut$purepl
    numlots <- plspot$numlots
    maxpos <- plspot$maxpos
    slipval <- plfut$slipval
    #calculate differences
    restab$Date[j] <- dateend
    restab$pl[j] <- pl
    restab$purepl[j] <- purepl
   # print(c(pureplnoswaps, pureplnoswapsprev))
  #  print(dateend)
  #  print(restab)
    restab$pureplnoswaps[j] <- pureplnoswaps
    restab$numlots[j] <- numlots
    restab$maxpos[j] <- maxpos
    restab$slipval[j] <- slipval
    restab$curpos[j] <- swaptab$Pos[swaptab$Date == dateend]
  #  plrev <- pl
  #  pureplprev <- purepl
  #  pureplnoswapsprev <- pureplnoswaps
  }
#print(swaptab)
#if (naflag) {
  names(extrafuttab) <- c("Date", "MaxPosPerDay", "TurnoverPerDay")
  restab2 <- data.table(Date = restab$Date, pl = restab$pl)
  setkey(extrafuttab, "Date")
  setkey(restab2, "Date")
  extrafuttab = extrafuttab[restab2]
  extrafuttab[is.na(MaxPosPerDay), MaxPosPerDay := 0]
  extrafuttab[is.na(TurnoverPerDay), TurnoverPerDay := 0]
  extrafuttab[, pl := c(pl[1], diff(pl))]
  return(list(res=restab, tabquotes=tabquotes, tabswaps=tabswaps, extrafuttab = extrafuttab))
#} else {
#  return(list(res=restab))
#}
#swaptab
}
PlCalc <- function(deals, tabquotes, mod) {
  if (mod != 1000) {
    deals[, slippage := ifelse(Direction == "Sell",
                               as.numeric(Comment) - Price,
                               Price - as.numeric(Comment))]
  } else {
    deals[, slippage := NA]
  }
  if (nrow(deals) == 0) {
    return(list(purepl = 0, pl = 0, numlots = 0))
  }
  finpos <- deals$Pos[nrow(deals)]
  pl <- deals[, sum(ifelse(Direction == "Sell", abs(Volume), -abs(Volume))*Price)] +
    ifelse(finpos>0, tabquotes$Bid[1]*abs(finpos), -tabquotes$Ask[1]*abs(finpos))
  commis <- deals[, sum(Commission)]
  numlots <- deals[, sum(abs(Volume))]
  maxpos <- deals[, max(abs(Pos))]
  sliptab <- deals[, sum(abs(Volume)*slippage)/sum(abs(Volume)), by = list(Date)]
  slipval <- deals[, sum(abs(Volume)*slippage)/sum(abs(Volume))]
 # print(tail(deals))
#  print(summary(deals$slippage))
#  print(sum(deals$Volume*deals$slippage)/sum(deals$Volume))
#  print(slipval)
#  print(sliptab)
return(list(purepl = pl*mod, pl = pl*mod - commis, numlots = numlots, maxpos = maxpos,
            slipval = slipval))
}
AddSwaps <- function(dates, deals, tabswaps) {
  deals <- as.data.table(as.data.frame(deals))
  len <- length(dates)
  restab <- data.frame(Date = rep(as.Date(origin), len),
                       Pos = numeric(len),
                       SwapVal = numeric(len))
  for (j in 1:len) {
    adate <- as.Date(dates[j])
    curswaps <- tabswaps[Date == adate, ]
    alldealsbefore <- deals[Date < adate, ]
    if (nrow(alldealsbefore) == 0) {
      poscur <- 0
     # dircur <- "Sell"
    } else {
      poscur <- alldealsbefore$Pos[nrow(alldealsbefore)]
    #  dircur <- ifelse(poscur )
    }
    restab$Date[j] <- as.Date(adate)
    restab$Pos[j] <- poscur
    if (nrow(curswaps) == 0) {
      restab$SwapVal[j] <- 0
    } else {
      restab$SwapVal[j] <- ifelse(poscur<0,
                                  curswaps$Bid[1]*abs(poscur),
                                  -curswaps$Ask[1]*abs(poscur))
    }
  }
  restab$SwapVal <- cumsum(restab$SwapVal)*1000
restab
}

