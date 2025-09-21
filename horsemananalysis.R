PrepareForSpreadCalc <- function(tab) {
  times <- data.table(time = unique(tab$Time))
  setkey(times, "time")
  tab[, DateCancel := fastPOSIXct(DateCancel, tz = "GMT")]
  tabsell <- tab[Side == "Sell", list(Time, DateCancel, Price)]
  tabbuy <- tab[Side == "Buy", list(Time, DateCancel, Price)]
  tabsell[, Date := Time]
  tabbuy[, Date := Time]
  setkey(tabsell, Time)
  setkey(tabbuy, Time)
  mtabsell <- tabsell[times, roll = TRUE, mult = "first"]
  mtabbuy <- tabbuy[times, roll = TRUE, mult = "first"]

  print(nrow(mtabsell))
  print(nrow(mtabbuy))

#  print(names(mtabsell))
  mtabsell[DateCancel < Time, Price := NA]
  mtabbuy[DateCancel < Time, Price := NA]
list(times = times, sell = mtabsell, buy = mtabbuy)
}

DateSpreadAnalysis <- function(tab) {
  fun <- function(tab) {
    print(tab$Date[1])
    mtab <- PrepareForSpreadCalc(tab)
    vec <- mtab$sell$Price - mtab$buy$Price
    vec <- vec[!is.na(vec)]
    rr <- mean(vec)
    print(rr)
    len <- length(vec)
    if (len > 0) {
      bs <- BootStrapMeanEstimate(vec)
      return(c(rr, len, len/nrow(tab), bs$conf.int[1], bs$conf.int[2]))
    }
    return(c(rr, len, len/nrow(tab), NA, NA))
  }
  tab[, {temp = fun(data.table(Date = Date, DateCancel = DateCancel, Price = Price, Time = Time, Side = Side)); print(temp);
  list(spreadestimate = temp[1], len = temp[2], ratio = temp[3], left = temp[4], right = temp[5])}, by = Date]
}

GetFutBidAskSpread <- function(datestart, dateend) {
  res <- data.frame(Date = Sys.Date(), Val = NA)
  for (adate in seq(datestart, dateend, 1)) {
    adate <- as.Date(adate)
    print(adate)
    tab <- na.omit(LoadBp(instrument = "SIZ5@FORTS", start.date = adate, end.date = adate, candle.type = "Analysis", storage.path = "D:"))
    if (nrow(tab)>0) {
      vec <- tab$Ask - tab$Bid
      print(summary(vec))
      res <- rbind(res, data.frame(Date = adate, Val = mean(vec)))
    }
  }
na.omit(res)
}
CheckShiftFunction <- function(tab, stavdat, beta, alpha1, alpha2) {
  tab$daystillexpir <- as.numeric(as.Date("2015-12-15") - tab$Date)
  days <- unique(tab$daystillexpir)
  dates <- unique(tab$Date)
  for (i in 1:length(dates)) {
    day <- days[i]
    adate <- as.Date(dates[i])
    print(day)
    alpha <- alpha1*day + alpha2
    print(alpha)
    temp <- ShiftFunctionAnalysis(tab, stavdat, adate, c(alpha, beta))
  }
}
ShiftFunctionAnalysis <- function(tab, stavdat, adate, kefs = NA, curfutname = "SIZ5@FORTS") {
  tab <- tab[Date == adate, ]
  stavdat <- stavdat[Date == adate, ]
  tabfut <- na.omit(LoadBp(curfutname, adate, adate, "Analysis", storage.path = "D:"))
  #names(tab) <- paste("orders", names(tab), sep = ".")
  names(stavdat) <- paste("deals", names(stavdat), sep = ".")
  names(tabfut) <- paste("fut", names(tabfut), sep = ".")
  #setkey(tab, "orders.Time")
  #setkey(stavdat, "deals.Time")
  setkey(tabfut, "fut.Time")
  ltab <- PrepareForSpreadCalc(tab)
  ltabsell <- ltab$sell
  ltabbuy <- ltab$buy
  names(ltabsell) <- paste("sell", names(ltab$sell), sep = ".")
  names(ltabbuy) <- paste("buy", names(ltab$buy), sep = ".")
  ltab <- cbind(ltabsell, ltabbuy)
  ltab <- ltab[!is.na(sell.Price) & !is.na(buy.Price), ]
  setkey(ltab, "sell.Time")
  setkey(stavdat, "deals.Time")
  mtab1 <- ltab[stavdat, roll = TRUE, mult = "first"]
  mtab2 <- tabfut[mtab1, roll = TRUE, mult = "first"]
  mtab2[, ":="(val = (sell.Price + buy.Price)/2, fut.Close = (fut.Bid + fut.Ask)/2)]
  if (is.na(kefs)) {
    kefs <- lm(I(mtab2$val - mtab2$fut.Close/1000 ~ mtab2$deals.pos))
    print(kefs)
  } else {print(kefs)}
  jpeg(paste0("regpict_", as.character(as.Date(adate)), ".jpeg"), width = 1900, height = 1200)
  plot(mtab2$deals.pos, mtab2$val - mtab2$fut.Close/1000)
  abline(kefs, col = "red")
  dev.off()
 return(list(mtab2 = mtab2, kefs = coef(kefs)))
}
ShiftFunctionMainAnalysis <- function(tab, stavdat, returnfullinfo = FALSE, curfutname = "SIZ5@FORTS") {
  dates <- unique(tab$Date)
  restab <- data.frame(date = rep(Sys.Date(), length(dates)), val1 = numeric(length(dates)), val2 = numeric(length(dates)))
  largetab <- c()
  for (i in 1:length(dates)) {
    adate <- as.Date(dates[i])
    print(adate)
    temp <- ShiftFunctionAnalysis(tab, stavdat, adate, curfutname = curfutname)
    restab$date[i] <- adate
    restab$val1[i] <- temp$kefs[1]
    restab$val2[i] <- temp$kefs[2]*1000
    if (returnfullinfo) {
      largetab <- rbind(largetab, rbind(c(), temp$mtab2))
    }
  }
if (returnfullinfo) {
  return(list(largetab = largetab, restab = restab))
} else {return(na.omit(restab))}
}
GetZeroPoint <- function(tab, stavdat, alpha1, alpha2, beta) {
  ltab <- PrepareForSpreadCalc(tab)
  ltabsell <- ltab$sell
  ltabbuy <- ltab$buy
  names(ltabsell) <- paste("sell", names(ltab$sell), sep = ".")
  names(ltabbuy) <- paste("buy", names(ltab$buy), sep = ".")
  ltab <- cbind(ltabsell, ltabbuy)
  ltab <- ltab[!is.na(sell.Price) & !is.na(buy.Price), ]
  setkey(ltab, "sell.Time")
  ltab[, day := as.numeric(as.Date("2015-12-15") - as.Date(sell.Date))]
  ltab[, alpha := alpha1*day + alpha2]
  names(stavdat) <- paste("deals", names(stavdat), sep = ".")
  setkey(stavdat, "deals.Time")
  mtab <- ltab[stavdat, roll = TRUE, mult = "first"]
mtab[, shift := alpha + deals.pos*beta]
}
PlotHorsemanDat <- function(tabfut, mtab, day0) {
  mtabred <- mtab[mtab$day == day0, ]
  tabfutred <- tabfut[tabfut$day == day0, ]
  print(nrow(mtabred))
  print(nrow(tabfutred))
  print(summary(mtabred))
  print(summary(tabfutred))
  plot(tabfutred$Time, tabfutred$Close/1000, type = "l")
  points(mtabred$sell.Time, (mtabred$sell.Price + mtabred$buy.Price)/2 - mtabred$shift, col = "red")
}

