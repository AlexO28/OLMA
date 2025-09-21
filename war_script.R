options(digits.secs = 3)
SetGlobalOptions()
###requires Comparison.R

GetWarDeals <- function(storage.path) {
  deals <- fread(storage.path, sep = ";", dec = ",", select = 3:8)
  names(deals) <- c("Time", "Security", "Volume", "Price", "Direction", "Comment")
  deals[, Time := as.POSIXct(Time, format = "%d-%m-%Y %H:%M:%OS")]
  deals[, Date := as.Date(Time)]
  deals[, Price := as.numeric(Price)]
}
GetWarDealsGlued <- function() {
  dealsarb <- GetWarDeals("D:\\arbdeals.txt")
  dealsmo <- GetWarDeals("D:\\modeals.txt")
  dealsextra <- GetWarDeals("D:\\extradeals.txt")
  deals <- rbind(dealsarb, dealsmo, dealsextra)
  setkey(deals, "Time")
  deals[Security == "USD000000TOD@CETS", Security := "USD000UTSTOM@CETS"]
deals[Date >= as.Date("2016-05-26"), ]
}
GetWarDealsAlt <- function(storage.path) {
  deals <- fread(storage.path, sep = ";", dec = ",", select = c(2, 4, 7, 8, 9, 10))
  names(deals) <- c("Time", "Security", "Price", "Volume", "Direction", "Comment")
  deals[, Time := as.POSIXct(Time)]
  deals[, Date := as.Date(Time)]
  deals[, Price := as.numeric(Price)]
  deals[, Side := Direction]
  names(deals) <- tolower(names(deals))
#  deals[, id := 1:nrow(deals)]
setkey(deals, "time")
}
AnalyzeWarDeals <- function(deals, ids = NA, fullanalysis = TRUE, war = TRUE, curfutname,
                            cursecname = "USD000UTSTOM@CETS", weights = c(1, 1), onlytablerequired = FALSE) {
  deals1 <- deals[Security == cursecname, ]
  deals2 <- deals[Security == curfutname, ]
  deals1[, Volume := weights[1]*as.numeric(Volume)*ifelse(Direction == "Buy", 1, -1)]
  deals2[, Volume := weights[2]*as.numeric(Volume)*ifelse(Direction == "Buy", 1, -1)]
  deals2[, Price := Price/1000]
  deals2[, id := 1:nrow(deals2)]
#  deals <- InnerMergeDf(list(usd = deals1, si = deals2), "Time")
#  if (!is.na(ids)) {
#    deals <- deals[-ids, ]
#  }
  print("before hedge comparison 2")
  print(summary(deals1))
  print(summary(deals2))
  deals <- HedgeComparison2(list(deals = deals1, orders = deals2), 0.001)
  print(summary(deals))
  mtab <- GetHedgeDiff(list(deals = deals1, orders = deals2), deals, fullmode = TRUE)
  print(summary(mtab))
  print(mtab$val)
  mtab <- mtab$tab
  if (onlytablerequired) {
    return(mtab)
  }
  #print(names(mtab))
  #print(summary(mtab))
  if (war) {
    print(names(mtab))
    names(mtab) <- c("usd.Time", "usd.Security", "usd.Volume", "usd.Price", "usd.Direction", "usd.Comment",
                   "usd.Date", "usd.hedgeid", "usd.hedgeidmin",
                   "si.Time", "si.Security", "si.Volume",
                   "si.Price", "si.Direction", "si.Comment", "si.Date", "si.id", "diff")
  } else {
    names(mtab) <- c("usd.Time", "usd.Security", "usd.Direction", "usd.Price", "usd.Volume",
                     "usd.Date", "usd.hedgeid", "usd.hedgeidmin",
                     "si.Time", "si.Security", "si.Direction",
                     "si.Price", "si.Volume", "si.Date", "si.id", "diff")
  }
  deals <- as.data.table(mtab)
  deals[, stavka := (si.Price - usd.Price)]
  if (fullanalysis == TRUE) {
    deals[, sign := abs(si.Volume)*ifelse(si.Direction == "Buy", 1, -1)]
    deals[, pos := cumsum(sign)]
    print(head(deals$pos))
    deals[, id := 1:nrow(deals)]
    finpos <- deals$pos[nrow(deals)]
    print(finpos)
    deals <- ClosePosWar(deals, finpos)
    deals$si.Volume[(nrow(deals) - abs(finpos) + 1):nrow(deals)] <- 1
    deals$usd.Volume[(nrow(deals) - abs(finpos) + 1):nrow(deals)] <- 1
    deals[, sign := abs(si.Volume)*ifelse(si.Direction == "Buy", 1, -1)]
    deals[, pos := cumsum(sign)]
    deals[, pl := cumsum(-sign*stavka), ]
  }
  deals[, deltatime := as.numeric(si.Time - usd.Time)]
}
RoughAnalysisWarDeals <- function(deals, posstart = 0, dateend = NA, curfutname = "SIM6@FORTS") {
  fulldeals <- as.data.table(as.data.frame(deals))
  if (is.na(dateend)) {
    dateend <- as.Date(fulldeals[nrow(fulldeals), Date])
  }
  fulldeals <- fulldeals[Date <= dateend, ]
  setkey(fulldeals, "Time")
  fulldeals[, Volume := ifelse(Direction == "Sell", -Volume, Volume)]
  fulldeals1 <- fulldeals[Security == "USD000UTSTOM@CETS", ]
  fulldeals2 <- fulldeals[Security == curfutname,]
  vol1 <- fulldeals1[, sum(abs(Volume))]
  vol2 <- fulldeals2[, sum(abs(Volume))]
#  fulldeals1[, Volume := Volume + posstart]
#  fulldeals2[, Volume := Volume - posstart]
  fulldeals2[, Price := Price/1000]
  fulldeals1[, id := 1:nrow(fulldeals1)]
  len  <- nrow(fulldeals1)
  fulldeals2[, id := 1:nrow(fulldeals2)]
  pos1 <- fulldeals1[, sum(Volume)] + posstart
  pos2 <- fulldeals2[, sum(Volume)] - posstart
  print(c(pos1, pos2))
  if (pos1 != 0) {
    fulldeals1 <- ClosePosWar(fulldeals1, pos1, "Direction")
  }
  if (pos2 != 0) {
    fulldeals2 <- ClosePosWar(fulldeals2, pos2, "Direction")
  }
  fulldeals1[Direction == "Sell" & Volume > 0, Volume := - Volume]
  prof1 <- fulldeals1[, sum(-Volume*Price)]
  fulldeals2[Direction == "Sell" & Volume > 0, Volume := - Volume]
  prof2 <- fulldeals2[, sum(-Volume*Price)]
  print("volumes")
  print(c(vol1, vol2))
  print(paste0("pl", prof1 + prof2))
  print(paste0("pl with commission", prof1 + prof2 - vol1*0.0005 - vol2*0.00025))
  print(paste0("pl with large commission", prof1 + prof2 - vol2*0.00025 - len*0.025))
  print("commis size:")
  print(vol2*0.00025 + len*0.025)
  list(spot = fulldeals1, fut = fulldeals2)
}
ClosePosWar <- function(deals, finpos, aname = "si.Direction") {
  vec <- deals[, aname, with = FALSE]
  if (finpos < 0) {
    tempvals <- deals$id[vec == "Buy"]
  } else if (finpos > 0) {
    tempvals <- deals$id[vec == "Sell"]
  } else {tempvals <- NA}
  lastid <- tempvals[length(tempvals)]
  for (j in 1:abs(finpos)) {
    deals <- rbind(deals, deals[lastid, ])
  }
  if (aname == "si.Direction") {
    deals[(nrow(deals) - abs(finpos) + 1):nrow(deals), si.Volume := 1]
    deals[(nrow(deals) - abs(finpos) + 1):nrow(deals), usd.Volume := 1]
  } else if (aname == "Direction") {
    deals[(nrow(deals) - abs(finpos) + 1):nrow(deals), Volume := 1]
    deals[(nrow(deals) - abs(finpos) + 1):nrow(deals), Volume := 1]
  }
deals
}
ProcessWarDeals <- function(datestart, dateend, endingdeals = NA, rough = FALSE, simulator = FALSE, posstart = 0) {
  if (!simulator) {
    fulldeals <- GetWarDeals("D:\\deals.txt")
  } else {
    fulldeals <- fread("C:\\ArbitrazhyorSimulation\\Reports\\2016-02-29- 18-10-56\\trades.csv")
    fulldeals[, Time := fastPOSIXct(Time)]
  }
  fulldeals$Date <- as.Date(fulldeals$Time)
  fulldeals <- fulldeals[(Date >= as.Date(datestart)) & (Date <= as.Date(dateend)), ]
  setkey(fulldeals, "Time")
  if (!is.na(endingdeals)) {
    fulldeals <- rbind(fulldeals, endingdeals)
  }
  if (!rough) {
    return(AnalyzeWarDeals(fulldeals))
  } else {
    return(RoughAnalysisWarDeals(fulldeals))
  }
}
EstimatePLBySecurity <- function(deals, secname, pricebid, priceask) {
  dealsred <- deals[security == secname, ]
  finpos <- deals[, sum(volume*ifelse(direction == "Buy", 1, -1))]
  myrpl <- dealsred[, sum(price*ifelse(direction == "Sell", 1, -1))]
  myupl <- 0
  if (finpos > 0) {
    myupl <- finpos*pricebid
  } else if (finpos < 0) {
    myupl <- -finpos*priceask
  }
myrpl + myupl
}
CalculateSlippageInWar <- function(deals, datestart, curfutname = "SIM6@FORTS") {
  deals <- deals[Security == curfutname & Date >= as.Date(datestart), ]
  slipstrut <- data.table(Date = as.Date(numeric(nrow(deals))), price = numeric(nrow(deals)), idealprice = numeric(nrow(deals)),
                          slippage = numeric(nrow(deals)))
  print(nrow(slipstrut))
 # for (j in 1:nrow(deals)) {
#    if (j %% 1000 == 0) {print(j)}
#   # slipstrut$Date[j] <- deals$Date[j]
#    slipstrut$price[j] <- deals$Price[j]
#    slipstrut$idealprice[j] <- idealprices[j]
#    if (deals$Direction[j] == "Sell") {
#      slipstrut$slippage[j] <- -slipstrut$price[j] + slipstrut$idealprice[j]
#    } else {
#      slipstrut$slippage[j] <- slipstrut$price[j] - slipstrut$idealprice[j]
#    }
#  }
  slipstrut$price <- deals$Price
  slipstrut$Date <- deals$Date
  slipstrut$idealprice <- as.numeric(substr(deals$Comment, 1, 5))
  slipstrut$slippage <- ifelse(deals$Direction == "Sell", slipstrut$idealprice - slipstrut$price, slipstrut$price - slipstrut$idealprice)
  slipstrut$volume <- deals$Volume
  slipstrut$Time <- deals$Time
slipstrut
}
CalculateSlippageForTrader <- function(trader, withaggregation = TRUE, secname = "SIM6@FORTS", fun = "sum") {
  storage.path <- paste0("D:\\ValuteTraders\\", trader, "\\deals.txt")
  deals <- GetWarDeals(storage.path)
  slipstrut <- na.omit(CalculateSlippageInWar(deals, as.Date("2016-03-24"), curfutname = secname))
  if (withaggregation) {
    if (fun == "sum") {
      slipstrutred1 <- aggregate(slipstrut$slippage*slipstrut$volume, list(slipstrut$Date), fun)
      slipstrutred2 <- aggregate(slipstrut$volume, list(slipstrut$Date), fun)
      return(data.frame(Date = slipstrutred1[, 1], Val = slipstrutred1[, 2]/slipstrutred2[, 2]))
    } else if (fun == "max") {
      slipstrutred1 <- aggregate(slipstrut$slippage, list(slipstrut$Date), max)
      return(data.frame(Date = slipstrutred1[, 1], Val = slipstrutred1[, 2]))
    }
  } else {
    return(slipstrut)
  }
}
PlotSlippageForTrader <- function(trader) {
  slipstruct <- CalculateSlippageForTrader(trader)
  jpeg(paste0("slippage", trader, ".jpeg"), width = 1900, height = 1200)
  plot(slipstruct, main = paste("average slippage for", trader, "is", median(slipstruct[, 2])))
  abline(h = median(slipstruct[, 2]))
  dev.off()
}
CalculateSlippageForBenderFile <- function(slipfilepath) {
  tab <- fread(slipfilepath, dec = ",")
  tab[Security == "USD000UTSTOM@CETS", ":="(DealPrice = DealPrice*1000, TargetPrice = TargetPrice*1000)]
  tab[, slippage := ifelse(Direction == "Buy", DealPrice - TargetPrice, TargetPrice - DealPrice)]
  write.table(tab, slipfilepath, append = FALSE, sep = ";", dec = ",", row.names = FALSE)
}
