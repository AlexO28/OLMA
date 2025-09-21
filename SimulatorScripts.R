Sys.setenv(TZ = "GMT")

CalculatePos <- function(tab, poses = c(0, 0), profs = c(0, 0), curfutname = "SIZ5@FORTS") {
  tab[, Pos := NA]
  tab[, RPL := NA]
  tabdol <- tab[Security %in% c("USD000UTSTOM@CETS", "USD000000TOD@CETS"), ]
  tabsi <- tab[Security == curfutname, ]
  CalculatePosServe <- function(tabdol, pos, prof) {
    setkey(tabdol, "Time")
    tabdol[, Pos := cumsum(Volume*ifelse(Direction == "Buy", 1, -1)) + pos]
    tabdol[, RPL := cumsum(Volume*ifelse(Direction == "Sell", 1, -1)*Price) + prof]
  }
  tabdol <- CalculatePosServe(tabdol, poses[1], profs[1])
  tabsi <- CalculatePosServe(tabsi, poses[2], profs[2])
  tab <- rbind(tabdol, tabsi)
  setorder(tab, Time, -Security)
}
CreateEmptyTradeTab <- function() {
data.table(Time = character(),
           Security = character(),
           Direction = character(),
           Price = numeric(),
           Volume = numeric(),
           Pos = numeric(),
           RPL = numeric())
}
GetQuotesTab <- function(datestart, dateend, closetime = "23:40:00", curfutname = "SIZ5@FORTS",
                         candletype = "10sVol50") {
  if (candletype == "Data") {
    storagepath <- "\\\\192.168.1.204\\share\\People\\Алексей\\"
  } else {
    storage.path <- "D:\\"
  }
  tabdol <- LoadBp("USD000UTSTOM@CETS", datestart, dateend, candle.type = candletype,
                   storage.path = storagepath,
                   start.time = closetime, end.time = closetime)
  tabfut <- LoadBp(curfutname, datestart, dateend, candle.type = candletype, storage.path = storagepath,
                   start.time = closetime, end.time = closetime)
  print(str(tabfut))
list(dol = tabdol, fut = tabfut)
}
GetSwapData <- function(datestart, dateend, atime = "10:05:00", candletype = "10sVol50") {
  if (candletype == "Data") {
    storagepath <- "\\\\192.168.1.204\\share\\People\\Алексей\\"
  } else {
    storage.path <- "D:\\"
  }
  tabswap <- LoadBp("USD000TODTOM@CETS", datestart, dateend, candle.type = candletype, storage.path = storagepath,
                      start.time = atime, end.time = atime)
  tabswap[Date == as.Date("2017-02-27"), Bid := 0.015]
  tabswap[Date == as.Date("2017-02-27"), Ask := 0.0151]
tabswap
}
GetSwapDataAlt <- function(datestart, dateend, atime = "10:05:00", candletype = "1sVol50") {
  storagepath <- "D:\\"
  tabtod <- LoadBp("USD000000TOD@CETS", datestart, dateend, candle.type = candletype, storage.path = storagepath,
                   start.time = atime, end.time = atime)
  tabtom <- LoadBp("USD000UTSTOM@CETS", datestart, dateend, candle.type = candletype, storage.path = storagepath,
                   start.time = atime, end.time = atime)
  tabtom <- tabtom[Date %in% tabtod$Date, ]
  tabswap <- data.table(tabtom$Time, tabtom$Bid - tabtod$Ask, tabtom$Ask - tabtod$Bid, tabtom$Date)
  names(tabswap) <- c("Time", "Bid", "Ask", "Date")
  tabswap[, Close := (Bid + Ask)/2]
tabswap
}
CalculateDayData <- function(adate, globinfo = list(pos = 0,
                                                    maxpos = 0,
                                                    prof = 0,
                                                    dealsprofdol = 0,
                                                    dealsproffut = 0,
                                                    trades = CreateEmptyTradeTab()),
                             quotestabdol, quotestabfut,
                             storagepath = "C:\\ArbitrazhyorSimulation\\Reports\\", curtrades = NA,
                             curfutname = "SIZ5@FORTS", tabswap = NA, reverse = FALSE) {
#get current data
 # reverse <- FALSE
  print("day")
  print(as.Date(adate))
  if (is.na(curtrades)) {
    afile <- file.path(storagepath, as.character(adate), "trades.csv")
    curtrades <- fread(afile, select = c(1, 4, 5, 6, 7))
  }
  swapmodifier <- 0
  print(tabswap)
  #if (nrow(tabswap)>0 & (tabswap)>0 & !is.na(tabswap)) {
  if (nrow(tabswap)>0) {
    tabswapred <- tabswap[Date == adate, ]
    if (nrow(tabswapred) > 0) {
      swapmodifier <- ifelse(globinfo$pos<0, -abs(globinfo$pos)*tabswapred$Ask[1], abs(globinfo$pos)*tabswapred$Bid[1])
    }
  }
  print(swapmodifier)
  if (!reverse) {
    curtrades <- CalculatePos(curtrades, poses = c(-globinfo$pos, globinfo$pos), profs = c(globinfo$dealsprofdol, globinfo$dealsproffut), curfutname = curfutname)
  } else {
    curtrades <- CalculatePos(curtrades, poses = c(globinfo$pos, -globinfo$pos), profs = c(globinfo$dealsprofdol, globinfo$dealsproffut), curfutname = curfutname)
  }
  curtradesdol <- curtrades[Security %in% c("USD000UTSTOM@CETS", "USD000000TOD@CETS"), ]
  commisdol <- (49/50)*sum(abs(curtradesdol$Volume))
  curtradesfut <- curtrades[Security == curfutname, ]
#I had 33/50 and 0.25 before
  if (nrow(curtradesfut) > 0) {
    commisfut <- (0.5)*sum(abs(curtradesfut$Volume))
  } else {
    commisfut <- 0
  }
  #relposes <- curtradesdol$Pos - globinfo$pos
  if (!reverse) {
    if (nrow(curtradesfut) > 0) {
      relposes <- (curtradesfut$Pos - globinfo$pos)
    } else {
       relposes <- 0
    }
  } else {
    if (nrow(curtradesdol) > 0) {
      relposes <- (curtradesdol$Pos - globinfo$pos)
    } else {
      relposes <- 0
    }
  }
  relmaxpos <- max(abs(relposes))
  if (!reverse) {
    if (nrow(curtradesfut)>0) {
      maxpos <- max(abs(curtradesfut$Pos))
    } else {
      maxpos <- abs(globinfo$pos)
    }
  } else {
    if (nrow(curtradesdol)>0) {
      maxpos <- max(abs(curtradesdol$Pos))
    } else {
      maxpos <- abs(globinfo$pos)
    }
  }
  posloc <- relposes[length(relposes)]
  if (!reverse) {
    if (nrow(curtradesfut) > 0) {
      posglob <- curtradesfut$Pos[nrow(curtradesfut)]
    } else {
      posglob <- globinfo$pos
    }
  } else {
    if (nrow(curtradesdol) > 0) {
      if (!(reverse)) {
        posglob <- -curtradesdol$Pos[nrow(curtradesdol)]
      } else {
        posglob <- curtradesdol$Pos[nrow(curtradesdol)]
      }
    } else {
      posglob <- globinfo$pos
    }
  }
  if (!reverse) {
    if (nrow(curtradesfut) > 0) {
      rplglob <- curtradesdol$RPL[nrow(curtradesdol)]*1000 + curtradesfut$RPL[nrow(curtradesfut)]
    } else {
      rplglob <- curtradesdol$RPL[nrow(curtradesdol)]*1000
    }
  } else {
    if (nrow(curtradesdol) > 0) {
      rplglob <- curtradesdol$RPL[nrow(curtradesdol)]*1000 + curtradesfut$RPL[nrow(curtradesfut)]
    } else {
      rplglob <- curtradesfut$RPL[nrow(curtradesfut)]
    }
  }
  ###rplloc <- rplglob - globinfo$dealsprofdol - globinfo$dealsproffut
  quotestabdolday <- quotestabdol[Date == adate, ]
  quotestabfutday <- quotestabfut[Date == adate, ]

  print(quotestabdol)
  print(quotestabfut)
  print("****")

  ###uplloc <- ifelse(posloc > 0, quotestabdolday$Bid[1]*posloc - quotestabfutday$Ask[1]*posloc,
  ###                 quotestabdolday$Ask[1]*posloc - quotestabfutday$Bid[1]*posloc)

  if (!(reverse)) {
    uplglob <- ifelse(posglob > 0, -quotestabdolday$Bid[1]*posglob*1000 + quotestabfutday$Ask[1]*posglob,
                    -quotestabdolday$Ask[1]*posglob*1000 + quotestabfutday$Bid[1]*posglob)
  } else {
    uplglob <- ifelse(posglob < 0, -quotestabdolday$Bid[1]*posglob*1000 + quotestabfutday$Ask[1]*posglob,
                      -quotestabdolday$Ask[1]*posglob*1000 + quotestabfutday$Bid[1]*posglob)
  }
  profglob <- rplglob + uplglob
  locinfo <- list(maxposloc = relmaxpos, profloc = profglob - globinfo$prof,
                  commisdol = commisdol, commisfut = commisfut, swap = swapmodifier,
                  numofdoldeals = nrow(curtradesdol), numofdeals = nrow(curtrades))

  if (ncol(globinfo$trades) > 7) {
    globinfo <- list(pos = posglob, maxpos = maxpos, prof = profglob,
                   dealsprofdol = curtradesdol$RPL[nrow(curtradesdol)],
                   dealsproffut = curtradesfut$RPL[nrow(curtradesfut)],
                   trades = rbind(globinfo$trades, curtrades))
  } else {
    globinfo <- list(pos = posglob, maxpos = maxpos, prof = profglob,
                     dealsprofdol = curtradesdol$RPL[nrow(curtradesdol)],
                     dealsproffut = curtradesfut$RPL[nrow(curtradesfut)],
                     trades = curtrades)
  }
  print(str(locinfo))
  print(str(globinfo))
list(loc = locinfo, glob = globinfo)
}
CalculateData <- function(dates, quotestabdol, quotestabfut) {
  simres <- CalculateDayData(dates[1], quotestabdol = quotestabdol, quotestabfut = quotestabfut)
  print(paste(dates[1], simres$glob$pos, simres$glob$prof))
  if (length(dates) > 1) {
    for (adate in dates[2:length(dates)]) {
      adate <- as.Date(adate)
      simres <- CalculateDayData(adate, simres$glob, quotestabdol, quotestabfut)
      print(paste(as.Date(adate), simres$glob$pos, simres$glob$prof))
    }
  }
simres
}
CalculateTimeIntervalData <- function(storagepath, quotestabdol, quotestabfut, curfutname = "SIZ5@FORTS", tabswap = NA, hack = FALSE, reverse = FALSE,
                                      datestart = NA, dateend = NA) {
  tab <- fread(storagepath, select = c(1, 3, 4, 5, 6))
  if (hack) {tab <- tab[3:nrow(tab), ]}
  tab[, Time := fastPOSIXct(Time, tz = "GMT")]
  tab[, Date := as.Date(Time)]
  if (!is.na(datestart) & !is.na(dateend)) {
    tab <- tab[Date >= datestart & Date <= dateend, ]
  }
  tab[, Price := as.numeric(gsub(",", ".", Price))]
  dates <- unique(tab$Date)
  globinfo <- list(pos = 0,
                   maxpos = 0,
                   prof = 0,
                   dealsprofdol = 0,
                   dealsproffut = 0,
                   trades = CreateEmptyTradeTab())
  restab <- data.frame(date = Sys.Date(), maxposloc = NA, profloc = NA, pos = NA,
                       maxpos = NA, prof = NA, commis = NA, swap = NA,
                       numofdoldeals = NA, numofdeals = NA)
  for (adate in dates) {
print(adate)
    print(as.Date(adate))
    print("starting")
 #   if (adate > as.Date("2016-03-29")) {stop()}
    tabred <- tab[Date == adate, ]
    if (!("USD000UTSTOM@CETS" %in% unique(tabred$Security))) {
      tabred <- rbind(tabred, data.table(
        Time = tabred$Time[1], Security = "USD000UTSTOM@CETS", Direction = "Buy", Price = 0, Volume = 0, Date = tabred$Date[1]
        ))
    }
    simres <- CalculateDayData(adate, globinfo, quotestabdol, quotestabfut, curtrades = tabred,
                               curfutname = curfutname, tabswap = tabswap, reverse = reverse)
    locinfo <- simres$loc
    globinfo <- simres$glob
    restab <- rbind(restab, data.frame(date = as.Date(adate), maxposloc = locinfo$maxposloc, profloc = locinfo$profloc,
                                       pos = globinfo$pos, maxpos = globinfo$maxpos, prof = globinfo$prof,
                                       commis = locinfo$commisdol + locinfo$commisfut, swap = locinfo$swap,
                                       numofdoldeals = locinfo$numofdoldeals, numofdeals = locinfo$numofdeals))
  }
list(res = na.omit(restab), simres = simres)
}
WriteZeroPointsForArbitrazhor <- function(rtab, id, mod = 1000, pricestep = 1) {
  rtab[, 2] <- round(mod*rtab[, 2]/pricestep)
  rtab[, 1] <- format(rtab[, 1], format = "%d.%m.%Y")
  rtab <- data.frame(V1 = rtab[, 1], V2 = rtab[, 2], V3 = ";;;")
  write.table(rtab[, 1:3], paste0("restab", id, ".csv"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
}
WriteZeroPointsForArbitrazhorSpreads <- function(rtab, id, spread0, mod = 1000, pricestep = 1) {
  rtab$Val <- round(mod*rtab$Val/pricestep)
  rtab$Date <- format(rtab$Date, format = "%d.%m.%Y")
  initspread <- round(15/abs(pricestep))
  rtab$Spread <- spread0 + round(initspread*(rtab$newdiffs-1)/(rtab$newdiffs[1] - 1))
  rtab$Shifts <- ";;"
  rtab <- as.data.frame(cbind(rtab$Date, rtab$Val, rtab$Spread, rtab$Shifts))
  write.table(rtab, paste0("restab", id, spread0, ".csv"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
}
WriteZeroPointsForArbitrazhorShifts <- function(rtab, id, shift0, mod = 1000, pricestep = 1) {
  rtab$Val <- round(mod*rtab$Val/pricestep)
  rtab$Date <- format(rtab$Date, format = "%d.%m.%Y")
  initspread <- round(15/abs(pricestep))
  rtab$fakespread <- -initspread*(rtab$newdiffs-1)/(rtab$newdiffs[1] - 1)
  rtab$ShiftLot <- ";1"
  rtab$ShiftVolume <- round(1000/(1000/shift0 + rtab$fakespread))
  rtab <- as.data.frame(cbind(rtab$Date, rtab$Val, rtab$ShiftLot, rtab$ShiftVolume))
  write.table(rtab, paste0("restab", id, shift0, ".csv"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
}
WriteZeroPointsAndQuantilesForArbitrazhor <- function(rtab, qtab, id, mod = 1000, pricestep = 2.5) {
  rtab <- as.data.frame(rtab)
  qtab <- as.data.frame(qtab)
  stab <- data.frame(Date = as.character(format(rtab[, 1], format = "%d.%m.%Y")),
                     model = round(rtab[, 2]*mod/pricestep),
                     spread = qtab$myspread,
                     ShiftLot = qtab$ShiftLot,
                     ShiftVolume = round(qtab$ShiftVolume)
                     )
  #return(stab)
  write.table(stab, paste0("restab", id, ".csv"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
}
WriteZeroPointsForArbitrazhorTransp <- function(restab, mod, id, pricestep = 1) {
  restab[, 2] <- round(-restab[, 2]*mod/pricestep)
  restab[, 1] <- format(restab[, 1], format = "%d.%m.%Y")
  write.table(restab[, 1:2], paste0("restab", id, ".csv"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
}
FillZeroDataForArbitrazhor <- function(datestart, dateend, alpha1, alpha2, datecrit) {
  dates <- as.Date(seq(as.Date(datestart), as.Date(dateend), 1))
  dates <- dates[!(as.POSIXlt(dates)$wday %in% c(0, 6))]
  vec <- as.numeric(as.Date(datecrit)) - as.numeric(dates)
  print(head(vec))
  vals <- alpha1*vec + alpha2
  rtab <- data.frame(Date = dates, Val = vals)
}
FillZeroDataForArbitrazhorByAvAnnModel <- function(tab, datestart, dateend) {
  dates <- tab$Date[tab$Date >= datestart & tab$Date <= dateend]
  dtab <- aggregate(tab$avannmodel*tab$newdiffs, list(dates), mean)
  data.frame(Date = format(dtab[, 1], format = "%d.%m.%Y"), Val = round(-1000*(dtab[, 2])))
}
CalculateSlippageInSimulator <- function(report = NA, deals = NA, download = TRUE, curfutname = "SIH6@FORTS") {
  if (download) {
    deals <- fread(paste0(report, "\\trades.csv"))
  }
  deals <- deals[Security == curfutname, ]
  dealssell <- deals[Direction == "Sell", ]
  dealsbuy <- deals[Direction == "Buy", ]
  slipstrut <- list(v1 = dealssell$Price, v2 = as.numeric(substr(dealssell$Comment, 1, 5)), vol1 = dealssell$Volume,
     v3 = dealsbuy$Price, v4 = as.numeric(substr(dealsbuy$Comment, 1, 5)), vol2 = dealsbuy$Volume)
  print((slipstrut))
  myslippage <- (sum((slipstrut$v2 - slipstrut$v1)*slipstrut$vol1) +
    sum((slipstrut$v3 - slipstrut$v4)*slipstrut$vol2))/(sum(slipstrut$vol1) + sum(slipstrut$vol2))
}
PlotPosaDifference <- function(report, DEALS, onone = FALSE) {
  deals <- as.data.table(DEALS)
  deals <- deals[Security == "USD000UTSTOM@CETS", ]
  trades <- fread(paste0(report, "\\trades.csv"))
  trades <- trades[Security == "USD000UTSTOM@CETS", ]
  trades[, Volume := ifelse(Direction == "Buy", 1, -1)*Volume]
  trades[, Pos := cumsum(Volume)]
  if (onone) {trades[, Pos := Pos*50]}
  trades[, Time := fastPOSIXct(Time)]
  deals[, Volume := ifelse(Direction == "Buy", 1, -1)*Volume]
  deals[, Pos := cumsum(Volume)]
  layout(1)
  plot(deals$Time, deals$Pos, ylab = "Reality", ylim = c(-150, 150), type = "b")
  points(trades$Time, trades$Pos, ylab = "Simulator", col = "red", pch = 24)
  lines(trades$Time, trades$Pos, ylab = "Simulator", col = "red")
  legend("topright", col = c("black", "red"), legend = c("reality", "simulator"), lty = 1)
list(t = trades, d = deals)
}
ChangePriceBySlippage <- function(pricevec, comvec, sidevec, slipval, reverse = FALSE) {
  if (reverse) {
    ifelse(sidevec == "Buy",
           signif(as.numeric(substr(sub(",", ".", comvec), 1, 7)) + slipval, 6),
           signif(as.numeric(substr(sub(",", ".", comvec), 1, 7)) - slipval, 6))
  } else {
    ifelse(sidevec == "Buy",
         signif(as.numeric(substr(comvec, 1, 5)) + slipval, 5),
         signif(as.numeric(substr(comvec, 1, 5)) - slipval, 5))
  }
}
ChangePriceBySlippageWALLE <- function(pricevec, comvec, sidevec, slipval, len=5) {
  barposes <- regexpr("|", comvec, fixed = TRUE)
  if (len>2) {
    vec <- ifelse(sidevec == "Buy",
         signif(1000*as.numeric(substr(comvec, barposes-len-1, barposes-1)) + slipval, len),
         signif(1000*as.numeric(substr(comvec, barposes-len-1, barposes-1)) - slipval, len))
  } else {
    vec <- ifelse(sidevec == "Buy",
                  signif(1000*as.numeric(substr(comvec, barposes-2, barposes-1)) + slipval, 2),
                  signif(1000*as.numeric(substr(comvec, barposes-2, barposes-1)) - slipval, 2))
  }
  if (length(vec[is.na(vec)])>0) {
    if (len>2) {
      vec[is.na(vec)] <- ChangePriceBySlippageWALLE(pricevec[is.na(vec)], comvec[is.na(vec)], sidevec[is.na(vec)], slipval, len-1)
    } else if (len == 2) {
      vec[is.na(vec)] <- ChangePriceBySlippageWALLE(pricevec[is.na(vec)], comvec[is.na(vec)], sidevec[is.na(vec)], slipval, 2)
    } else {
      print(vec[is.na(vec)])
      stop()
    }
  }
vec
}
FillNewSlippageInReport <- function(report, slipval, curfut, wallemode = FALSE, reverse = FALSE) {
  deals <- fread(as.character(report), sep = ";")
  if (wallemode) {
    deals[Security == curfut, Price := ChangePriceBySlippageWALLE(Price, Comment, Direction, slipval)]
  } else {
    deals[Security == curfut, Price := ChangePriceBySlippage(Price, Comment, Direction, slipval, reverse = reverse)]
  }
  print(head(deals))
  write.table(deals, report, quote = FALSE, sep = ";", row.names = FALSE)
}
###structure of indstruct:
###field index (list of columns of valtab)
###valtab
CreateSampleIndTab <- function() {
  list(indices = c(5, 26), valtab = data.frame(rep(c("30", "33", "37", "42"), 2), rep(c("5", "10"), 4)))
}
FillOptimTab <- function(reportpath, rdspath, indstruct) {
  optimtab <- as.data.frame(readRDS(rdspath))
  optimvec <- optimtab[, 4]
  optimtab <- optimtab[, 1:3]
  for (j in 1:nrow(indstruct$valtab)) {
    optimtab[, j+3] <- optimvec
    for (k in 1:ncol(indstruct$valtab)) {
      optimtab[indstruct$indices[k], j+3] <- as.character(indstruct$valtab[j, k])
    }
  }
  write.table(optimtab, reportpath, sep = ";", quote = TRUE, row.names = FALSE)
optimtab
}
DefineFileById <- function(optimid, mystr = "trades.csv") {
#modified code
  mystr <- paste0("\\", mystr)
#end of modified code
  if (optimid < 10) {
    optimfile <- paste0("0000", optimid, mystr)
  } else if (optimid < 100) {
    optimfile <- paste0("000", optimid, mystr)
  } else if (optimid < 1000) {
    optimfile <- paste0("00", optimid, mystr)
  } else if (optimid < 10000) {
    optimfile <- paste0("0", optimid, mystr)
  } else {
    optimfile <- paste0(optimid, mystr)
  }
optimfile
}
FillNewSlippageInOptim <- function(reportpath, slipval, curfut, wallemode, reverse = FALSE, optimids = NA) {
  if (is.na(optimids)) {
    optimres <- as.data.frame(fread(paste0(reportpath, "optimizerResult.csv")))
    optimids <- optimres[, ncol(optimres)]
  }
  for (optimid in optimids) {
    optimfile <- DefineFileById(optimid)
    print(optimfile)
    if (reverse) {
      rrr <- FillNewSlippageInReport(paste0(reportpath, "IterationsResult\\", optimfile), slipval, "USD000UTSTOM@CETS", wallemode = wallemode, reverse = reverse)
    } else {
      rrr <- FillNewSlippageInReport(paste0(reportpath, "IterationsResult\\", optimfile), slipval, curfut, wallemode = wallemode, reverse = reverse)
    }
  }
}
CalculateOptimResults <- function(reportpath, quotestab, curfutname, tabswap, bank, hack = FALSE, slipval, reverse = FALSE,
                                  datestart = NA, dateend = NA, optimids = NA) {
  if (is.na(optimids)) {
    optimres <- as.data.frame(fread(paste0(reportpath, "optimizerResult.csv")))
    optimids <- optimres[, ncol(optimres)]
  }
  cumres <- data.frame(id = numeric(), prof = numeric(), maxpos = numeric(), numdeals = numeric(),
                       numdoldeals = numeric(), dohodnost = numeric(), sharp = numeric(), slipval = numeric(),
                       numoftradeddays = numeric())
  for (optimid in optimids) {
    optimfile <- DefineFileById(optimid)
    print(optimfile)
    res <- CalculateTimeIntervalData(paste0(reportpath, "IterationsResult\\", optimfile), quotestab$dol, quotestab$fut,
                                     curfutname = curfutname, tabswap = tabswap, hack = hack, reverse = reverse,
                                     datestart = datestart, dateend = dateend)
    res <- AnalyzeSimResults(res)
    mypath <- DefineFileById(optimid, paste0("result", slipval, ".csv"))
    write.table(res, paste0(reportpath, "IterationsResult\\", mypath), quote = FALSE, sep = ";", row.names = FALSE)
    cumres <- rbind(cumres, data.frame(id = optimid, prof = sum(res$profloc), maxpos = max(abs(res$maxposloc)),
                                       numdeals = sum(res$numofdeals), numdoldeals = sum(res$numofdoldeals),
                                       dohodnost = 100*250*mean(res$profloc)/(bank),
                                       sharp = mean(res$profloc)/sd(res$profloc), slipal = slipval,
                                       numoftradeddays = mean(res$numoftradeddays)))
  }
  write.table(cumres, paste0(reportpath, "fullresult.csv"), sep = ";", append = TRUE, row.names = FALSE, quote = FALSE)
cumres
}
AnalyzeSimResults <- function(resold) {
  res <- resold$res
  data.frame(date = res$date, maxposloc = res$maxpos,
             pureprofloc = res$profloc, commis = res$commis, swap = res$swap*1000,
             profloc = res$profloc - res$commis + res$swap*1000,
             numofdoldeals = res$numofdoldeals, numofdeals = res$numofdeals, numoftradeddays = length(unique(res$date)))
#  data.frame(date = res$date, maxposloc = res$maxpos*50,
#             pureprofloc = res$profloc*50, commis = res$commis*50, swap = res$swap*50000,
#             profloc = res$profloc*50 - res$commis*50 + res$swap*50000,
#             numofdoldeals = res$numofdoldeals, numofdeals = res$numofdeals, numoftradeddays = length(unique(res$date)))
}
FullSimAnalysis <- function(report, slipval, curfut, datestart, dateend, bank = 6000000, downloaddata = TRUE, downloadswaps = TRUE, tab = NA, tabswap = NA, wallemode = FALSE,
                            reverse = FALSE, optimids = NA) {
  if (downloaddata) {
    tab <- GetQuotesTab(datestart, dateend, closetime = "23:40:00",
                        curfutname = curfut, candletype = "Data")
  }
  if (downloadswaps) {
    tabswap <- GetSwapData(datestart, dateend, atime = "10:15:00", candletype = "Data")
  }
 # rrr <- FillNewSlippageInOptim(report, slipval, curfut, wallemode = wallemode, reverse = reverse,
#                                optimids = optimids)
  print("filled slippage")
  optim <- CalculateOptimResults(report, tab, curfut, tabswap, bank, FALSE, slipval, reverse = reverse, datestart = datestart, dateend = dateend,
                                 optimids = optimids)
}
