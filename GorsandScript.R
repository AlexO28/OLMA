GorsandWriteToFile <- function(filestr, tab) {
  tab$Date <- format(as.Date(tab$Date), "%d.%m.%Y")
  tab$Val <- round(tab$Val)
  print(head(tab))
  write.table(tab, filestr, sep = ";", row.names = FALSE, quote = FALSE)
}
GorsandGetPriceShift <- function(datestart, dateend, curspotname, curfutname, nextdate, critdate, contangofactor) {
  tab <- FillStavkaTab(datestart, dateend, spotsec = curspotname, futsec = curfutname,
                       candletype = "Data", storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\", mod = 1000)
  dates <- unique(tab$Date)
  tab[, stavka := 50*1000*stavka]
print(summary(tab$stavka))
  firstval <- tab[Date == dates[1], mean(stavka)]
  restab <- data.frame(Date = dates[1], Val = NA, FirstVal = NA, MeanVal = NA,
                       diff1 = NA, diff2 = NA, contango = NA)
                      # Val2 = NA, Val3 = NA)
  for (j in 2:length(dates)) {
    meanval <- tab[Date == dates[j-1], mean(stavka)]
    if (j<length(dates)) {
      datenext <- as.numeric(dates[j+1])
      if (datenext %in% c(as.Date("2016-07-04"), as.Date("2016-09-05"), as.Date("2016-10-10"))) {
        datenext <- datenext + 1
      }
    } else {
      datenext <- nextdate
    }
    val <- firstval - meanval + contangofactor*
      meanval*(as.numeric(datenext) - as.numeric(dates[j]))/(as.numeric(critdate) - as.numeric(dates[j]) + 1)
    restab <- rbind(restab, data.frame(Date = dates[j], Val = val,
                                       FirstVal = firstval,
                                       MeanVal = meanval,
                                       diff1=as.numeric(datenext) - as.numeric(dates[j]),
                                       diff2=as.numeric(critdate) - as.numeric(dates[j]) + 1,
                                       contango = contangofactor
                                       ))
                                      # Val2 = firstval - meanval))
                                      # Val3 = contangofactor*
                                      #   meanval*(as.numeric(datenext) - as.numeric(dates[j]))/(as.numeric(critdate) - as.numeric(dates[j]) + 1)))
  }
restab
}
GorsandMain <- function(gordeals, gormod, slipvals, tabswap, tabquotes, curfutname, returnmode = FALSE) {
  dat <- GorsandProcessData(gordeals, gormod, curfutname = curfutname)
  if (!is.na(slipvals)) {
    dat <- GorsandFillSlippage(dat, slipvals[1], "usd", curfutname = curfutname)
    dat <- GorsandFillSlippage(dat, slipvals[2], "si", curfutname = curfutname)
  }
GorsandCalculateStat(dat, tabswap, tabquotes, returnmode = returnmode)
}
GorsandCalculateStat <- function(dat, tabswap, tabquotes, returnmode = FALSE) {
  tab <- dat$usd
  swapplinfo <- GorsandCalculateSwapPl(tab, tabswap)
  print(swapplinfo)
  plinfo <- GorsandCalculatePl(dat, tabquotes)
  swappl <- swapplinfo$val
  pl <- plinfo$val
  swaptab <- as.data.table(swapplinfo$tab)
  pltab <- as.data.table(plinfo$tab)
  setkey(swaptab, Date)
  setkey(pltab, Date)
  bigtab <- swaptab[pltab]
  bigtab[is.na(Val), Val := 0]
  print(bigtab)
  bigtab[is.na(Val), Val := 0]
  bigtab[, Val := Val + i.Val]
  print(bigtab)
  if (returnmode) {return(bigtab)}
  plot(bigtab$Date, bigtab$Val, type = "l")
  medprof <- median(diff(bigtab$Val))
  meanprof <- mean(diff(bigtab$Val))
  sharp <- meanprof/sd(diff(bigtab$Val))
  numswapdays <- nrow(tabswap)
  #for simulator
  #tab[, Date := as.Date(fastPOSIXct(Time))]
  #for fight
  tab[, Date := as.Date(as.POSIXct(as.numeric(Time), origin = origin))]
  numtradeddays <- length(unique(tab[, Date]))
  numdeals <- nrow(tab)
  len <- as.numeric(tabswap$Date[nrow(tabswap)]) - as.numeric(tabswap$Date[1])
  fullpl <- pl + swappl
  profit <- 100*365*(fullpl)/((len+1)*50*12000)
list(fullpl = fullpl, profit = profit, pl = pl, swappl = swappl, numswapdays = numswapdays,
     numtradeddays = numtradeddays, numdeals = numdeals, len = len,
     medprof = 100*365*medprof/((1)*50*12000), meanprof = 100*365*meanprof/((1)*50*12000), sharp = sharp)
}
GorsandCalculateSwapPl <- function(tab, tabswap) {
  tab <- as.data.table(as.data.frame(tab))
  #tab <- as.data.frame(tab)
  #tab$mytime <- as.Date(as.POSIXct(as.numeric(tab$Time), origin = origin))
  #this one is for simulator analysis
#  tab[, Date := as.Date(fastPOSIXct(as.character(Time)))]
  tab[, Date := as.Date(as.POSIXct(as.numeric(Time), origin = origin))]
  print(head(tab))
  tab[, Pos := cumsum(ifelse(Direction == "Sell", -Volume, Volume))]
  swappl <- 0
  print(head(tab))
  pos <- 0
  restab <- data.frame(Date = Sys.Date(), Val = NA)
  for (j in 1:nrow(tabswap)) {
    adate <- tabswap$Date[j]
    tabred <- tab[Date < adate, ]
    if (nrow(tabred)>0) {
      pos <- tabred$Pos[nrow(tabred)]
      swappl <- swappl + abs(pos)*ifelse(pos>0, -tabswap$Ask[j], tabswap$Bid[j])
    }
    restab <- rbind(restab, data.frame(Date = adate, Val = swappl*1000))
    print(c(swappl, pos))
  }
list(val = 1000*swappl, tab = na.omit(restab))
}
GorsandCalculatePl <- function(dat, tabquotes) {
  tabdol <- dat$usd
  tabsi <- dat$si
# this lines are for simulator
# tabdol[, Date := as.Date(fastPOSIXct(Time))]
#  tabsi[, Date := as.Date(fastPOSIXct(Time))]
# this lines are for the fight
  tabdol[, Date := as.Date(as.POSIXct(as.numeric(Time), origin = origin))]
  tabsi[, Date := as.Date(as.POSIXct(as.numeric(Time), origin = origin))]
  tabdol[, Pos := cumsum(ifelse(Direction == "Sell", -Volume, Volume))]
  tabsi[, Pos := cumsum(ifelse(Direction == "Sell", -Volume, Volume))]

  quotesdol <- tabquotes$dol
  quotessi <- tabquotes$fut
  restab <- data.frame(Date = Sys.Date(), Val = NA)
  for (j in 1:nrow(quotesdol)) {
    adate <- quotesdol$Date[j]
    print(adate)
    quotesdolred <- quotesdol[Date == adate, ]
    quotessired <- quotessi[Date == adate, ]
    tabdolred <- tabdol[Date <= adate, ]
    tabsired <- tabsi[Date <= adate, ]
    if (nrow(tabdolred) == 0) {
      restab <- rbind(restab, data.frame(Date = adate, Val = 0))
    } else {
      pldol <- tabdolred[, sum(Price*ifelse(Direction == "Buy", -Volume, Volume) - Commission/1000)]
      posdol <- tabdolred$Pos[nrow(tabdolred)]
      print(pldol)
      pldol <- pldol + abs(posdol)*ifelse(posdol>0, quotesdolred$Bid[nrow(quotesdolred)], -quotesdolred$Ask[nrow(quotesdolred)])
      print(pldol)
      print(quotesdolred)
      possi <- tabsired$Pos[nrow(tabsired)]
      plsi <- tabsired[, sum(Price*ifelse(Direction == "Buy", -Volume, Volume) - Commission)]
      print(plsi)
      plsi <- plsi + abs(possi)*ifelse(possi>0, quotessired$Bid[nrow(quotessired)], -quotessired$Ask[nrow(quotessired)])
      print(plsi)
      print(quotessired)
      fullpl <- plsi + pldol*1000
      print(c(pldol*1000, plsi, posdol, possi,  fullpl))
      restab <- rbind(restab, data.frame(Date = adate, Val = fullpl))
    }
  }
list(val = fullpl, tab = na.omit(restab))
}
GorsandFillSlippage <- function(dat, slipval, aname, curfutname) {
  if (aname == "usd") {
    tab <- dat$usd
    bname <- "USD0TargetPrice"
  }
  if (aname == "si") {
    tab <- dat$si
    bname <- paste0(substr(curfutname, 1, 4), "TargetPrice")
  }
  tab2 <- dat$mod
  print(head(tab2))
  print(bname)
  tab[Direction == "Sell", Price := tab2[tab$Direction == "Sell", bname, with = FALSE] - slipval]
  tab[Direction == "Buy", Price := tab2[tab$Direction == "Buy", bname, with = FALSE] + slipval]
  if (aname == "usd") {
    dat$usd <- tab
  }
  if (aname == "si") {
    dat$si <- tab
  }
dat
}
GorsandProcessData <- function(gordeals, gormod, curfutname) {
  gordeals <- as.data.table(as.data.frame(gordeals))
  gormod <- as.data.table(as.data.frame(gormod))
  gordeals[, TimeNum := as.numeric(fastPOSIXct(Time))]
  gormod[, TimeNum := as.numeric(fastPOSIXct(Time))]
  setkey(gordeals, "TimeNum")
  setkey(gormod, "TimeNum")
  gordealsusd <- gordeals[Security == "USD000UTSTOM@CETS", ]
  gordealsusd <- TransformFromCharacters(t(sapply(unique(gordealsusd$TimeNum), "GorsandProcessPerTimeMoment", gordeals = gordealsusd)))
  gordealssi <- gordeals[Security == curfutname, ]
  gordealssi <- TransformFromCharacters(t(sapply(unique(gordealssi$TimeNum), "GorsandProcessPerTimeMoment", gordeals = gordealssi)))
list(mod = gormod, usd = gordealsusd, si = gordealssi)
}
GorsandProcessPerTimeMoment <- function(atime, gordeals) {
  gordealsred <- gordeals[TimeNum == atime, ]
  data.frame(TimeNum = gordealsred$TimeNum[1],
             Time = gordealsred$Time[1],
             Direction = gordealsred$Direction[1],
             Price = gordealsred[, sum(Price*Volume)/sum(Volume)],
             Volume = gordealsred[, sum(Volume)],
             Commission = gordealsred[, sum(Commission)])
}
TransformFromCharacters <- function(tab) {
  data.table(TimeNum = as.numeric(tab[, 1]),
             Time = tab[, 2],
             Direction = tab[, 3],
             Price = as.numeric(tab[, 4]),
             Volume = as.numeric(tab[, 5]),
             Commission = as.numeric(tab[, 6]))
}
