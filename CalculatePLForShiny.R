library(RODBC)
library(data.table)
source("C:\\Saule\\Comparison.R")
source("C:\\Saule\\BrandNewHedge.R")

options(stringsAsFactors = FALSE)
options(digits.secs = 3)
origin <- "1970-01-01"

Sys.setenv(TZ = "GMT")


Sys.setlocale(,"ru_RU")

listofassets1 <- list("NA" = 1, "USD000UTSTOM@CETS" = 2,
                               "EUR_RUB__TOM@CETS" = 5,
                               "SBER@TQBR" = 8,
                               "GAZP@TQBR" = 11,
                               "LKOH@TQBR" = 14, 
                               "VTBR@TQBR" = 17,
                               "MXM6@FORTS" = 20, "MXU6@FORTS" = 21,
                               "MMM6@FORTS" = 22, "MMU6@FORTS" = 23,
                               "RIM6@FORTS" = 24, "RIU6@FORTS" = 25)
listofassets2 <- list("NA" = 1, "SI@FORTS" = 0, "EU@FORTS" = -1, "SIM6@FORTS" = 3, "SIU6@FORTS" = 4, 
                               "EUM6@FORTS" = 6, "EUU6@FORTS" = 7,
                               "SRM6@FORTS" = 9, "SRU6@FORTS" = 10,
                               "GZM6@FORTS" = 12, "GZU6@FORTS" = 13,
                               "LKM6@FORTS" = 15, "LKU6@FORTS" = 16,
                               "VBM6@FORTS" = 18, "VBU6@FORTS" = 19,
                               "MXM6@FORTS" = 20, "MXU6@FORTS" = 21,
                               "MMM6@FORTS" = 22, "MMU6@FORTS" = 23,
                               "RIM6@FORTS" = 24, "RIU6@FORTS" = 25)
MyDateTransform <- function(adate) {
  gsub("-", "_", adate)
}
GetCurrentQuotes <- function(adate) {
  tabquotes <- try(fread(paste0("Z:\\Data\\Reports\\VolumeSpreads\\", MyDateTransform(adate), ".csv"), na.strings = "-1"))
  if (class(tabquotes) == "try-error") {
    return(data.frame(Time = numeric(), dol.bid = numeric(), dol.ask = numeric(),
           si.bid = numeric(), si.ask = numeric()))
  }
  tabquotes <- as.data.frame(tabquotes)
  for (i in c(7, 6, 3, 2)) {
    tabquotes[tabquotes[, i] == -1, i] <- NA
  }
  tabquotes <- na.omit(tabquotes[, 1:13])
print(tail(tabquotes))
  tabquotes <- tabquotes[nrow(tabquotes), ]
data.frame(Time = as.POSIXct(paste(adate, tabquotes[1, 1])),
           dol.bid = tabquotes[1, 7], dol.ask = tabquotes[1, 6],
           si.bid = tabquotes[1, 3], si.ask = tabquotes[1, 2])
}
MainSlippageCalc <- function(datestart, dateend, Sec1, Sec2, port1, port2, commentpattern1 = "", commentpattern2 = "") {
  Sec1 <- names(listofassets1[listofassets1 == Sec1])
  Sec2 <- names(listofassets2[listofassets2 == Sec2])
  deals <- GetDealsForMain(port1, port2, datestart, dateend, Sec1, Sec2, filtered = FALSE, 
                           commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  slipstrut <- CalculateSlippageInWar(deals$orders)
  res <- CalculateAggrSlipInfo(slipstrut)
  res$Date <- as.character(as.Date(res$Date))
res
}
CalculateSlippageInWar <- function(deals) {
  slipstrut <- data.table(Price = numeric(nrow(deals)))
  slipstrut$price <- deals$Price
  slipstrut$Date <- deals$Date
  slipstrut$idealprice <- as.numeric(substr(deals$Comment, 1, 5))
  slipstrut$slippage <- ifelse(deals$Direction == "Sell", slipstrut$idealprice - slipstrut$price, slipstrut$price - slipstrut$idealprice)
  slipstrut$volume <- deals$Volume
  slipstrut$Time <- deals$Time
na.omit(slipstrut)
}
CalculateAggrSlipInfo <- function(slipstrut) {
  dates <- unique(slipstrut$Date)
  setkey(slipstrut, Date)
  volsum <- as.data.frame(slipstrut[, sum(abs(volume)), by = Date])
  slipvolsum <- as.data.frame(slipstrut[, sum(abs(volume)*slippage), by = Date])
  dealscount <- as.data.frame(slipstrut[, length(volume), by = Date])
  slipmean <- as.data.frame(slipstrut[, mean(slippage), by = Date])
data.frame(Date = dates, WeightedSlippage = slipvolsum[, 2]/volsum[, 2], MeanSlippage = slipmean[, 2],
           Volume = volsum[, 2], NumberOfDeals = dealscount[, 2])
}
MainGetDealsTab <- function(control, datestart, dateend, Sec1, Sec2, port1, port2, commentpattern1 = "", commentpattern2 = "") {
  Sec1 <- names(listofassets1[listofassets1 == Sec1])
  Sec2 <- names(listofassets2[listofassets2 == Sec2])

print(Sec1)
print(Sec2)

  deals <- GetDealsForMain(port1, port2, datestart, dateend, Sec1, Sec2, filtered = FALSE, 
                           commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  if (as.numeric(control) == 1) {
    largedeals <- rbind(deals$deals, deals$orders)
    setkey(largedeals, "Time")
    largedeals$Time <- as.character(as.POSIXct(largedeals$Time))
    largedeals$Date <- as.character(as.Date(largedeals$Date))
    return(as.data.frame(largedeals))
  } else {
    res <- PrepareDealsFromBase(deals)
    setkey(res, "Time")
    res <- as.data.frame(res)
    res$Time <- as.character(as.POSIXct(res$Time))
    res$Date <- as.character(as.Date(res$Date))
    return(res)
  }
}
#служба учета
#4071183
#пр ударников 18А
#артем
PrepareDealsFromBase <- function(dat, mod = 1000, eps = NA) {
#  mod <- num2/num1
#  if (mod != 1000) {
#    weights <- c(100, 10)
#  }
  if (is.na(eps)) {
    deals <- HedgeComparison2(dat)
  } else {
    deals <- HedgeComparison2(dat, eps = eps)
  }
  mtab <- GetHedgeDiff(dat, deals)
  mtab <- mtab$tab
  if (is.na(eps)) {
    res <- data.frame(Date = mtab$Date, Time = mtab$Time, spotprice = mtab$Price, futprice = mtab$price,
            Side = mtab$Side, Volume = mtab$Volume, diff = mtab$price - mod*mtab$Price)
  } else {
    res <- data.frame(Date = mtab$date, Time = mtab$time, spotprice = mtab$price, futprice = mtab$Price,
            Side = mtab$side, Volume = mtab$volume, diff = mtab$Price - mod*mtab$price)
  }

  res$pos <- cumsum(-res$Volume)
#data.table(usd.Time = mtab$Time, usd.Security = mtab$Security, usd.Volume = mtab$Volume,
#           usd.Price = res$spotprice, usd.Direction = mtab$Side, usd.Date = mtab$Date,
#           usd.hedgeid = mtab$hedgeid, usd.hedgeidmin = mtab$hedgeidmin,
#           si.Time = mtab$time, si.Security = mtab$security, si.Volume = mtab$volume,
#           si.Price = res$futprice, si.Direction = mtab$side, si.Date = mtab$date,
#           si.id = mtab$id, diff = res$diff, stavka = res$futprice/mod - res$spotprice,
#           deltatime = as.numeric(mtab$time) - as.numeric(mtab$Time))
data.table(Time = mtab$Time,
           stavka = res$futprice/mod - res$spotprice, 
           usd.Security = mtab$Security, usd.Volume = mtab$Volume, usd.Price = res$spotprice, usd.Direction = mtab$Side,
           si.Security = mtab$security, si.Volume = mtab$volume, si.Price = res$futprice, si.Direction = mtab$side,
           deltatime = as.numeric(mtab$time) - as.numeric(mtab$Time),
           Date = mtab$Date
           )
}
MainPlCalc <- function(datestart, dateend, Sec1, Sec2, RadChoice, stavka, pos, port1, port2, 
                       commentpattern1 = "", commentpattern2 = "") {
  Sec1 <- names(listofassets1[listofassets1 == Sec1])
  Sec2 <- names(listofassets2[listofassets2 == Sec2])
  deals <- GetDealsForMain(port1, port2, datestart, dateend, Sec1, Sec2, commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)

  hres <- AnalysisWarDeals(deals$deals, deals$orders, datestart, dateend, RadChoice, stavka, pos)
  PlotAnalysis(hres)
}
MainPlCalcMod <- function(datestart, dateend, Sec1, Sec2, RadChoice, stavka, pos, port1, port2, 
                       commentpattern1 = "", commentpattern2 = "", bank = 0) {
  Sec1 <- names(listofassets1[listofassets1 == Sec1])
  Sec2 <- names(listofassets2[listofassets2 == Sec2])
  dat <- GetBrandNewDeals(port1, port2, datestart, dateend, Sec1, Sec2, commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  tabdoltom <- dat$tom
  tabdoltod <- dat$tod
  tabfut <- dat$fut
  if (nrow(tabdoltod) > 0) {
    res <- BrandNewFullMatch(tabdoltod, tabdoltom, fullmode = TRUE)
    print("out")
    swaptab <- res$res
    tabdol <- res$tab2 
    swapres <- AnalyzeTodTomDeals(swaptab, datestart)
  } else {
    tabdol <- tabdoltom
    swapres <- list(posstart = 0,swapprofit = 0)
  }
  tabquotes <- GetCurrentQuotes(dateend)
  if (nrow(tabquotes) > 0) {
    noclose <- TRUE
  } else {
    noclose <- FALSE
  }
  hres <- AnalysisWarDeals(tabdol, tabfut, datestart, dateend, RadChoice, stavka, swapres$posstart, noclose = noclose)
  if (nrow(tabquotes)>0) {
    posdol <- hres$finpos[1]
    possi <- hres$finpos[2]
    hres$plbyquotes <- 1000*ifelse(posdol>=0, tabquotes$dol.bid[nrow(tabquotes)]*abs(posdol), -tabquotes$dol.ask[nrow(tabquotes)]*abs(posdol)) +
                       ifelse(possi>=0, tabquotes$si.bid[nrow(tabquotes)]*abs(possi), -tabquotes$si.ask[nrow(tabquotes)]*abs(possi))
    hres$quotetime <- tabquotes$Time[nrow(tabquotes)]
    hres$stavka <- ifelse(possi>=0, tabquotes$si.bid[nrow(tabquotes)], tabquotes$si.ask[nrow(tabquotes)])/1000 -
                    ifelse(posdol>=0, tabquotes$dol.bid[nrow(tabquotes)], tabquotes$dol.ask[nrow(tabquotes)])
  }
  timelen <- 1 + dateend - datestart
  if (timelen <= 0) {
    bank <- 0
  }
  PlotAnalysisWithSwaps(hres, swapres$swapprofit, bank = bank, timelen = timelen)
}
MainPlCalcModFull <- function(datestart, dateend, Sec1, Sec2, RadChoice, stavka, pos, port1, port2, 
                       commentpattern1 = "", commentpattern2 = "") {
  Sec1 <- names(listofassets1[listofassets1 == Sec1])
  Sec2 <- names(listofassets2[listofassets2 == Sec2])
  dat <- GetBrandNewDeals(port1, port2, datestart, dateend, Sec1, Sec2, commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  tabdoltom <- dat$tom
  tabdoltod <- dat$tod
  tabfut <- dat$fut

  if (nrow(tabdoltod) > 0) {
    res <- BrandNewFullMatch(tabdoltod, tabdoltom, fullmode = TRUE)
    print("out")
    swaptab <- res$res
    tabdol <- res$tab2 
    swapres <- AnalyzeTodTomDeals(swaptab, datestart)
    print(swapres)
    res2 <- BrandNewFullMatch(tabdol, tabfut, fullmode = FALSE)
    print("dollar")
    print(sum(tabdol$Volume))
    print("fut")
    print(sum(tabfut$Volume))
    print("combo")
    print(sum(res2$usd.vol))
    print(sum(res2$si.vol)) 
    print(names(res2))
    res <- AnalyzeWithClassification(res2, tabdol, tabfut)
    res$swapres <- swapres
PlotAnalyzeWithClassification(res)
###res2
  }
}
PlotAnalyzeWithClassification <- function(res) {
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = "n", xaxt = "n", yaxt = "n")
print(res)
  #total statistics
  if (sum(res$truetotalprofit) + res$swapres$swapprofit <= 0) {
    acol <- "red"
  } else {
    acol <- "green"
  }
  text(x = 0.1, y = 0.95, paste0("Прибыль"), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = 0.95, round(sum(res$truetotalprofit) + res$swapres$swapprofit), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = 0.9, "Прибыль от свопов",  cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = 0.9, round(res$swapres$swapprofit),  cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = 0.85, paste0("Прибыль без свопов"), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = 0.85, round(sum(res$truetotalprofit)), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = 0.8, paste0("Прибыль без коммиссии и без свопов"), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = 0.8, round(sum(res$faketotalprofit)), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = 0.75, paste0("Количество сделок по доллару"), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = 0.75, res$num, cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = 0.7, "Поза по доллару", cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = 0.7, res$posdol + res$swapres$posstart, cex = 1.3, col = acol, adj = c(0, 0))
  step <- 0.05
  valstart <- 0.7-step
#  for (i in 1:length(res$seclist)) {
#    secur <- res$seclist[i]
#    text(x = 0.1, y = valstart, paste("Поза по", secur), cex = 1.3, col = acol, adj = c(0, 0))
#    text(x = 0.7, y = valstart, res$posfuts[i], cex = 1.3, col = acol, adj = c(0, 0))
#    valstart <- valstart - step
#  }
  for (i in 1:length(res$seclist)) {
    secur <- res$seclist[i]
    if (res$trueprofsaggr[i] <= 0) {
      acol <- "red"
    } else {
      acol <- "green"
    }
    text(x = 0.1, y = valstart - step, paste("Прибыль для ", secur), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = valstart - step, round(res$trueprofsaggr[i]), cex = 1.3, col = acol, adj = c(0, 0))
 #   text(x = 0.1, y = valstart - 2*step, paste("Рассклассифицировано по", secur), cex = 1.3, col = acol, adj = c(0, 0))
 #   text(x = 0.7, y = valstart - 2*step, paste0(posfutsaggr[i], " лотов"), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.1, y = valstart - 2*step, paste("Прибыль без коммиссии для", secur), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = valstart - 2*step, round(sum(res$fakeprofsaggr[i])), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.1, y = valstart - 3*step, paste("Количество сделок по доллару для", secur), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = valstart - 3*step, res$numaggr[i], cex = 1.3, col = acol, adj = c(0, 0))
    valstart <- valstart - 4*step
  }
  if (sum(res$trueproffutnevyazka) <= 0) {
    acol <- "red"
  } else {
    acol <- "green"
  }
  text(x = 0.1, y = valstart - step, paste0("Прибыль от нерасклассифицированного"), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = valstart - step, round(sum(res$trueproffutnevyazka)), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = valstart - 2*step, paste0("Всего не удалось рассклассифицировать"), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.7, y = valstart - 2*step, paste(sum(res$num - res$numaggr), "сделок по доллару"), cex = 1.3, col = acol, adj = c(0, 0))  
}
AnalyzeWithClassification <- function(res2, tabdol, tabfut) {
  tabfut <- as.data.table(as.data.frame(tabfut))
  tabdol <- as.data.table(as.data.frame(tabdol))
  res2 <- as.data.table(as.data.frame(res2))

  tabfut[, Security := substr(Security, 1, 4)]
  seclist <- unique(tabfut$Security)
  res2[, si.sec := substr(si.sec, 1, 4)]
  
  #total statistics dollar
  posdol <- tabdol[, sum(Volume)]
  num <- nrow(tabdol)
  numsell <- nrow(tabdol[Direction == "Sell", ])
  numbuy <- nrow(tabdol[Direction == "Buy", ])
  lastdolprice <- tabdol$Price[nrow(tabdol)]
  fakeprofdol <- tabdol[, sum(-1000*Price*Volume)]  + 1000*posdol*lastdolprice
  trueprofdol <- fakeprofdol - tabdol[, sum(Commission)]   

print("dollar")
print(posdol)
print(lastdolprice)
print(c(fakeprofdol, trueprofdol))


  posfuts <- c()
  lastfutprices <- c()
  fakeproffuts <- c()
  trueproffuts <- c()
  posfutsaggr <- c()
  posdolsaggr <- c()
  fakeproffutsaggr <- c()
  trueproffutsaggr <- c()
  fakeprofdolsaggr <- c()
  trueprofdolsaggr <- c()
  numaggr <- c()
  for (secur in seclist) {
    #total statistics for secur-futures
    tabfutred <- tabfut[Security == secur, ]
    posfut <- tabfutred[, sum(Volume)]
    posfuts <- c(posfuts, posfut)
    lastfutprice <- tabfutred$Price[nrow(tabfutred)]
    lastfutprices <- c(lastfutprices, lastfutprice)
    fakeproffut <- tabfutred[, sum(-Price*Volume)] + posfut*lastfutprice
    fakeproffuts <- c(fakeproffuts, fakeproffut)
    trueproffuts <- c(trueproffuts, fakeproffut - tabfutred[, sum(Commission)])
    
    #aggregated statistics for secur-futures (taken from res2)
    resred <- res2[si.sec == secur, ]
    posfutaggr <- resred[, sum(si.vol)]
    posfutsaggr <- c(posfutsaggr, posfutaggr)
    posdolaggr <- resred[, sum(usd.vol)]
    posdolsaggr <- c(posdolsaggr, posdolaggr)
    fakeproffutaggr <- resred[, sum(-si.price*si.vol)] + posfutaggr*lastfutprice
    trueproffutaggr <- fakeproffutaggr - resred[, sum(si.commis)] 
    fakeproffutsaggr <- c(fakeproffutsaggr, fakeproffutaggr)
    trueproffutsaggr <- c(trueproffutsaggr, trueproffutaggr)
    fakeprofdolaggr <- resred[, sum(-1000*usd.price*usd.vol)] + 1000*posdolaggr*lastdolprice
    trueprofdolaggr<- fakeprofdolaggr - resred[, sum(usd.commis)] 
    fakeprofdolsaggr <- c(fakeprofdolsaggr, fakeprofdolaggr)
    trueprofdolsaggr <- c(trueprofdolsaggr, trueprofdolaggr)
    numaggr <- c(numaggr, nrow(resred))
  }

print("futures")
print(posfuts)
print(lastfutprices)
print(fakeproffuts)
print(trueproffuts)
print("aggregated")
print(posfutsaggr)
print(posdolsaggr)
print(fakeproffutsaggr)
print(trueproffutsaggr)
print(fakeprofdolsaggr)
print(trueprofdolsaggr)
  
  #nevyazka statistics
  posdolnevyazka <- posdol - sum(posdolsaggr)
  fakeprofdolnevyazka <- fakeprofdol - sum(fakeprofdolsaggr)
  trueprofdolnevyazka <- trueprofdol - sum(trueprofdolsaggr)
  posfutnevyazka <- posfuts - posfutsaggr 
  fakeproffutnevyazka <- fakeproffuts - fakeproffutsaggr
  trueproffutnevyazka <- trueproffuts - trueproffutsaggr

print("nevyazka")
print(posdolnevyazka)
print(fakeprofdolnevyazka)
print(trueprofdolnevyazka)
print(posfutnevyazka)
print(fakeproffutnevyazka)
print(trueproffutnevyazka)

list(seclist = seclist,
     numaggr = numaggr, 
     num = num, numsell = numsell, numbuy = numbuy, lastdolprice = lastdolprice, lastfutprices = lastfutprices,
     faketotalprofit = c(fakeprofdol, fakeproffuts), truetotalprofit = c(trueprofdol, trueproffuts), posdol = posdol,
     posfuts = posfuts, posdolsaggr = posdolsaggr,
     posfutsaggr = posfutsaggr, fakeprofsaggr = fakeproffutsaggr + fakeprofdolsaggr, trueprofsaggr = trueproffutsaggr + trueprofdolsaggr,
     posdolnevyazka = posdolnevyazka, posfutnevyazka = posfutnevyazka,
     fakeproffutnevyazka = fakeprofdolnevyazka + fakeproffutnevyazka,
     trueproffutnevyazka = trueprofdolnevyazka + trueproffutnevyazka
    )
}
AnalyzeTodTomDeals <- function(swaptab, adate) {
  swaptab <- as.data.table(as.data.frame(swaptab))
  swaptab[, Date := as.Date(Time)]
  swaptabred <- swaptab[Date == adate, ]
  #here fut is tom, dol is tod
  posstart <- swaptabred[, sum(si.vol)]
  swapprofit <- swaptab[, sum(-1000*(si.vol*si.price+usd.vol*usd.price) - si.commis - usd.commis)]
list(posstart = posstart, swapprofit = swapprofit)
}
AnalysisWarDeals <- function(deals1, deals2, datestart, dateend, mode, stavka, pos, noclose = FALSE) {
  deals1 <- as.data.table(as.data.frame(deals1))
  deals2 <- as.data.table(as.data.frame(deals2))
  setkey(deals1, "Time")
  setkey(deals2, "Time")
  mod <- 1000
  deals2[, Price := Price/mod]
  #vol1 <- deals1[, sum(abs(Volume))]
  #vol2 <- deals2[, sum(abs(Volume))]
  pos1 <- deals1[, sum(Volume)]
  pos2 <- deals2[, sum(Volume)]
  dealmaxtime <- max(c(deals1$Time, deals2$Time))
  num <- nrow(deals1)
  numsell <- nrow(deals1[Direction == "Sell", ])
  numbuy <- nrow(deals1[Direction == "Buy", ]) 
  avbuy <- deals1[Direction == "Buy", sum(Price*Volume)/sum(Volume)]
  avsell <- deals1[Direction == "Sell", sum(Price*Volume)/sum(Volume)]
 # commis1 <- 0.0005 
 # commis2 <- 0.00025 

if (noclose == FALSE) {
  if (mode == 1) {
    price1 <- NA
    price2 <- NA
    if (pos1 != 0) {
      res <- ClosePosWar(deals1, pos1)
      deals1 <- res$deals
      price1 <- res$price
    } 
    if (pos2 != 0) {
      res <- ClosePosWar(deals2, pos2)
      deals2 <- res$deals
      price2 <- res$price
    }
    stavka <- price2 - price1
    nevyazka <- 0
  } else {
    nevyazka <- -stavka*pos1
  }
} else {
  nevyazka <- 0
}
  deals1[Direction == "Sell" & Volume > 0, Volume := - Volume]
  prof1 <- deals1[, sum(-Volume*Price)]
  deals2[Direction == "Sell" & Volume > 0, Volume := - Volume]
  prof2 <- deals2[, sum(-Volume*Price)]

  res <- list(fakepl = 1000*(prof1 + prof2) + 1000*nevyazka, truepl = 1000*(prof1 + prof2) - sum(deals1$Commission) - sum(deals2$Commission) + 1000*nevyazka,
              numofdeals = num, finpos = c(pos1+pos, pos2-pos), stavka = stavka, dealmaxtime = dealmaxtime, 
              numsell = numsell, numbuy = numbuy, avsell = avsell, avbuy = avbuy)
}
PlotAnalysisWithSwaps <- function(res, swapprofit, bank = 0, timelen = 0) {
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = "n", xaxt = "n", yaxt = "n")
  if (!is.null(res$plbyquotes)) {
    truepl0 <- res$truepl + res$plbyquotes  
  } else {
    truepl0 <- res$truepl
  }
  if (truepl0 + swapprofit <= 0) {
    acol <- "red"
  } else {
    acol <- "green"
  }
   if (!is.null(res$plbyquotes)) {
    text(x = 0.1, y = 0.95, paste0("Прибыль: "), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.95, round(truepl0 + swapprofit), cex = 1.3, col = acol, adj = c(0, 0))
   } else {
    text(x = 0.1, y = 0.95, paste0("Прибыль по сделкам: "), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.95, round(res$truepl + swapprofit), cex = 1.3, col = acol, adj = c(0, 0))
   }
    text(x = 0.1, y = 0.9, paste0("Прибыль без комиссии: "), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.9, round(truepl0 - (res$truepl - res$fakepl) + swapprofit), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.1, y = 0.85, paste0("Прибыль от своп-сделок"), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.85, round(swapprofit), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.1, y = 0.8, paste0("Прибыль от обычных сделок"), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.8, round(truepl0), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.1, y = 0.75, paste0("Поза по 1-му: "), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.75, round(res$finpos[1]), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.1, y = 0.7, paste0("Поза по 2-му: "), cex = 1.3, col = acol, adj = c(0, 0))
    text(x = 0.7, y = 0.7, round(res$finpos[2]), cex = 1.3, col = acol, adj = c(0, 0))
  text(x = 0.1, y = 0.6, "Сделок по 1-му инструменту", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.6, round(res$numofdeals), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.55, "Сделок на продажу по 1-му инструменту", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.55, round(res$numsell), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.5, "Сделок на куплю по 1-му инструменту", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.5, round(res$numbuy), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.45, "Средняя продажа", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.45, round(res$avsell, 5), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.4, "Средняя купля", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.4, round(res$avbuy, 5), cex = 1.3, col = "blue", adj = c(0, 0))
  if (!is.null(res$quotetime)) {
    text(x = 0.1, y = 0.3, "Поза закрыта по котировкам на", cex = 1.3, adj = c(0, 0))
    text(x = 0.7, y = 0.3, res$quotetime, cex = 1.3, adj = c(0, 0))
  } else {
    text(x = 0.1, y = 0.3, "Время последней сделки", cex = 1.3, adj = c(0, 0))
    text(x = 0.7, y = 0.3, res$dealmaxtime, cex = 1.3, adj = c(0, 0))
  }

  if (bank > 0) {
    text(x = 0.1, y = 0.2, "Доходность в процентах", cex = 1.3, adj = c(0, 0), col = acol)
    text(x = 0.7, y = 0.2, round((365*100/bank)*(truepl0 + swapprofit)/(as.numeric(timelen)), 2), cex = 1.3, adj = c(0, 0), col = acol)
  }

  if (res$finpos[2] < 0) {
    text(x = 0.5, y = 0.1, paste0("Поза закрыта куплей ставки по ", res$stavka), cex = 1.3, adj = c(0, 0))
  } else if (res$finpos[2] > 0) {
    text(x = 0.5, y = 0.1, paste0("Поза закрыта продажей ставки по ", res$stavka), cex = 1.3, adj = c(0, 0))
  }

}
PlotAnalysis <- function(res) {
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = "n", xaxt = "n", yaxt = "n")
  if (res$truepl <= 0) {
    text(x = 0.1, y = 0.95, paste0("Прибыль: "), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.7, y = 0.95, round(res$truepl), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.1, y = 0.9, paste0("Прибыль без комиссии: "), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.7, y = 0.9, round(res$fakepl), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.1, y = 0.85, paste0("Поза по 1-му: "), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.7, y = 0.85, round(res$finpos[1]), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.1, y = 0.8, paste0("Поза по 2-му: "), cex = 1.3, col = "red", adj = c(0, 0))
    text(x = 0.7, y = 0.8, round(res$finpos[2]), cex = 1.3, col = "red", adj = c(0, 0))
  } else {
    text(x = 0.1, y = 0.95, paste0("Прибыль: "), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.7, y = 0.95, round(res$truepl), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.1, y = 0.9, paste0("Прибыль без комиссии: "), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.7, y = 0.9, round(res$fakepl), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.1, y = 0.85, paste0("Поза по 1-му: "), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.7, y = 0.85, round(res$finpos[1]), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.1, y = 0.8, paste0("Поза по 2-му: "), cex = 1.3, col = "green", adj = c(0, 0))
    text(x = 0.7, y = 0.8, round(res$finpos[2]), cex = 1.3, col = "green", adj = c(0, 0))
  }
  text(x = 0.1, y = 0.7, "Сделок по 1-му инструменту", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.7, round(res$numofdeals), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.65, "Сделок на продажу по 1-му инструменту", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.65, round(res$numsell), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.6, "Сделок на куплю по 1-му инструменту", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.6, round(res$numbuy), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.55, "Средняя продажа", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.55, round(res$avsell, 5), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.5, "Средняя купля", cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.7, y = 0.5, round(res$avbuy, 5), cex = 1.3, col = "blue", adj = c(0, 0))
  text(x = 0.1, y = 0.4, "Время последней сделки", cex = 1.3, adj = c(0, 0))
  text(x = 0.7, y = 0.4, res$dealmaxtime, cex = 1.3, adj = c(0, 0))

  if (res$finpos[2] < 0) {
    text(x = 0.5, y = 0.2, paste0("Поза закрыта куплей ставки по ", res$stavka), cex = 1.3, adj = c(0, 0))
  } else if (res$finpos[2] > 0) {
    text(x = 0.5, y = 0.2, paste0("Поза закрыта продажей ставки по ", res$stavka), cex = 1.3, adj = c(0, 0))
  }
}
ClosePosWar <- function(deals, finpos) {
 # vec <- deals$Direction
 # if (finpos < 0) {
 #   tempvals <- deals$id[vec == "Buy"]
  #} else if (finpos > 0) {
 #   tempvals <- deals$id[vec == "Sell"]
 # } else {tempvals <- NA}
 # if (length(tempvals) == 0) {
   tempvals <- deals$id[nrow(deals)]
 # }
  lastid <- tempvals[length(tempvals)]
  laststr <- deals[lastid, ]
	
  if (finpos > 0) {
    laststr$Direction <- "Sell"
  } else {
    laststr$Direction <- "Buy"
  }
  laststr$Commission <- abs(laststr$Commission/laststr$Volume)
#print(finpos)
#print(laststr)
 # for (j in 1:abs(finpos)) {
 #   deals <- rbind(deals, laststr)
 # }
  deals <- rbind(deals, laststr[rep(1, abs(finpos)),])
  deals[(nrow(deals) - abs(finpos) + 1):nrow(deals), Volume := 1]
  deals[(nrow(deals) - abs(finpos) + 1):nrow(deals), Volume := 1]
print("close pos")
print(tail(deals))
list(deals = deals, price = deals$Price[lastid])
}
GetDealsForMain <- function(portfolio1, portfolio2, datestart, dateend, secname1, secname2, weights = NA, eps = NA, filtered = TRUE,
                            commentpattern1 = "", commentpattern2 = "") {
  dealsnew <- GetDealsFromBase(portfolio1, portfolio2, datestart, dateend)
  dealshist <- GetDealsFromBaseHistory(portfolio1, portfolio2, datestart, dateend)
  if (nrow(dealsnew) > 0) {
    datnew <- ProcessDealsFromBase(dealsnew, secname1, secname2, weights, eps, filtered = filtered, 
                                   commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  }
  if (nrow(dealshist) > 0) {
    dathist <- ProcessDealsFromBaseHistory(dealshist, secname1, secname2, weights, eps, filtered = filtered, 
                                           commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  }
print("finished")
  if (nrow(dealsnew) > 0 & nrow(dealshist) > 0) {
    dealshist <- dathist$deals
    dealsnew <- datnew$deals
    dealsnew <- dealsnew[!(dealsnew$Date %in% unique(dealshist$Date)), ] 
    ordershist <- dathist$orders
    ordersnew <- datnew$orders
#print(str(datnew$orders))
    ordersnew <- ordersnew[!(ordersnew$Date %in% unique(ordershist$Date)), ] 
#print(summary(ordersnew))

  }
  if (nrow(dealsnew) == 0 & nrow(dealshist) == 0) {
    stop("Нет сделок за этот период")
  } else  if (nrow(dealsnew) == 0){
    dat <- dathist
  } else if (nrow(dealshist) == 0) {
    dat <- datnew
  } else {

    tabdeals <- rbind(dealsnew, dealshist)
    taborders <- rbind(ordersnew, ordershist)
    setkey(tabdeals, "Time")
    setkey(taborders, "Time")
    tabdeals$id <- 1:nrow(tabdeals)
    taborders$id <- 1:nrow(taborders)
    dat <- list(deals = tabdeals, orders = taborders)
  }
dat
}
###taken from Helios
GetDealsFromBase <- function(portfolio1, portfolio2, datestart, dateend) {
  ch <- odbcConnect("mydata", "reader", "123")
  #constr <-  paste("select [Date], [Time], [SecurityCode], Price, Volume, Side from dbo.DBQuikTrades114 (nolock) where Portfolio =", portfolio1,
  #                 "or Portfolio =", portfolio2, "order by [Time]")
#  constr <- gsub("@d1", datestart, constr)
#  constr <- gsub("@d2", dateend, constr)
#  constr <- gsub("@p1", portfolio1,  constr)
#  constr <- gsub("@p2", portfolio2,  constr)
#print(constr)
#  tab0 <- sqlQuery(ch, constr)
  
  constr <- "select [Date], [Time], [SecurityCode], Price, Volume, Side, Commission, Comment from dbo.DBQuikTrades114 (nolock) where (Portfolio = '@p1'  or Portfolio = '@p2') 
            and (convert(Date, [Date], 103) >= '@d1') and  (convert(Date, [Date], 103) <= '@d2')"
    constr <- gsub("@d1", datestart, constr)
    constr <- gsub("@d2", dateend, constr)
    constr <- gsub("@p1", portfolio1,  constr)
    constr <- gsub("@p2", portfolio2,  constr)
    print(constr)
    tab <- sqlQuery(ch, constr)  
 constr2 <- "select [Date], [Time], [SecurityCode], Price, Volume, Side, Commission, Comment from dbo.DBQuikTrades113 (nolock) where (Portfolio = '@p1'  or Portfolio = '@p2') 
            and (convert(Date, [Date], 103) >= '@d1') and  (convert(Date, [Date], 103) <= '@d2')"
    constr2 <- gsub("@d1", datestart, constr2)
    constr2 <- gsub("@d2", dateend, constr2)
    constr2 <- gsub("@p1", portfolio1,  constr2)
    constr2 <- gsub("@p2", portfolio2,  constr2)
    print(constr2)
    tab2 <- sqlQuery(ch, constr2)  

  odbcClose(ch)
rbind(tab, tab2)
}
GetDealsFromBaseHistory <- function(portfolio1, portfolio2, datestart, dateend) {
  ch <- odbcConnect("mydata", "reader", "123")
  constr <- "select [Date], [Security], Price, Volume, Side, Commission, Comment from dbo.DBTrades (nolock) where (Portfolio = '@p1'  or Portfolio = '@p2') 
                       and ([Date] >= '@d1') and  ([Date] <= '@d2 23:59:59')"

  constr <- gsub("@d1", datestart, constr)
    constr <- gsub("@d2", dateend, constr)
    constr <- gsub("@p1", portfolio1,  constr)
    constr <- gsub("@p2", portfolio2,  constr)
    print(constr)
    tab <- sqlQuery(ch, constr)  

  odbcClose(ch)
tab
}
ProcessDealsFromBaseHistory <- function(tab, sec1, sec2, weights = NA, eps = NA, filtered = TRUE,
                                        commentpattern1 = "", commentpattern2 = "") {
print("process history")
  if (is.na(weights)) {
    weights <- c(1, 1)
  }

  tabmod <- data.frame(Time = as.POSIXct(tab$Date), 
                    Security = tab$Security, Price = tab$Price, Volume = tab$Volume, 
                    Side = tab$Side, Commission = tab$Commission, Comment = tab$Comment)
  if (nrow(tabmod) == 0) {
    return(tabmod)
  }
  tabmod$Security <- toupper(tabmod$Security)

print(unique(tabmod$Security))
print(summary(tabmod))

  tabmod <- as.data.table(tabmod)
  tabmod$Direction <- tabmod$Side
  tabmod[, ":="(Volume = Volume*ifelse(Side == "Buy", 1, -1), Date = as.Date(Time))]
  tabmod$Security <- toupper(tabmod$Security)
  if (filtered) {
    if (sec1 == "USD000UTSTOM@CETS") {
      tabmod[Security == "USD000000TOD@CETS", Security := sec1]
    } else if (sec1 == "EUR_RUB__TOM@CETS") {
      tabmod[Security == "EUR_RUB__TOD@CETS", Security := sec1]
    }
  }
  tabmod[Security == sec1, Volume := Volume*weights[1]]
  if (filtered) {
    if (sec2 == "SI@FORTS" | sec2 == "EU@FORTS") {
      tabmod[grepl(substr(sec2, 1, 2), Security) & grepl("@FORTS", Security), Security := sec2]
    } else {
      tabmod[grepl(substr(sec2, 1, 4), Security), Security := sec2]
    }
  }
  tabmod[Security == sec2, Volume := Volume*weights[2]]
  if (filtered == FALSE) {
    if (sec1 == "USD000UTSTOM@CETS") {
      tab1 <- tabmod[Security %in% c("USD000UTSTOM@CETS", "USD000000TOD@CETS", "USD000TODTOM@CETS"), ]
    } else if (sec1 == "EUR_RUB__TOM@CETS") {
      tab1 <- tabmod[Security %in% c("EUR_RUB__TOM@CETS", "EUR_RUB__TOD@CETS", "EUR000TODTOM@CETS")]
    }
    if (sec2 == "SI@FORTS" | sec2 == "EU@FORTS") {
      tab2 <- tabmod[grepl(substr(sec2, 1, 2), Security) & grepl("@FORTS", Security), ]
    } else {
      tab2 <- tabmod[grepl(substr(sec2, 1, 4), Security), ]
    }    
  } else {
    tab1 <- tabmod[Security == sec1, ]
    tab2 <- tabmod[Security == sec2, ]
  }

  tab1[, Volume := Volume*weights[1]]
  tab2[, Volume := Volume*weights[2]]
  tab1$Date <- as.Date(tab1$Time)
  tab2$Date <- as.Date(tab2$Time)

  if (commentpattern1 != "") {    
    tab1 <- tab1[grepl(commentpattern1, Comment), ]
  }
  if (commentpattern2 != "") {
    tab2 <- tab2[commentpattern2 == substr(Comment, 6, 6), ]
  }

  setkey(tab1, "Time")
  setkey(tab2, "Time")
  tab1[, id := 1:nrow(tab1)]
  tab2[, id := 1:nrow(tab2)]
  if (is.na(eps)) {
    return(list(deals = tab1, orders = tab2))
  } else {
    ###for stocks we need to reverse the data
    return(list(deals = tab2, orders = tab1))
  }
}
ProcessDealsFromBase <- function(tab, sec1, sec2, weights = NA, eps = NA, filtered = TRUE, commentpattern1 = "", commentpattern2 = "") {
print("process")
  if (is.na(weights)) {
    weights <- c(1, 1)
  }

  tabmod <- data.frame(Time = as.POSIXct(strptime(paste(tab$Date, tab$Time), "%d.%m.%Y %H:%M:%S")), 
                    Security = tab$SecurityCode, Price = tab$Price, Volume = tab$Volume, 
                    Side = ifelse(tab$Side == "Купля", "Buy", "Sell"), Commission = tab$Commission, 
                    Comment = tab$Comment)
  if (nrow(tabmod) == 0) {
    return(tabmod)
  }
  tabmod$Security <- toupper(tabmod$Security)
  tabmod <- as.data.table(tabmod)
  tabmod$Direction <- tabmod$Side
  tabmod[, ":="(Volume = Volume*ifelse(Side == "Buy", 1, -1), Date = as.Date(Time))]
print(c(sec1, sec2))
  sec1short <- strsplit(sec1, '@')[[1]][1]
  sec2short <- strsplit(sec2, '@')[[1]][1]
  tabmod$Security <- toupper(tabmod$Security)
  tabmod[Security == sec1short, Security := sec1]
  if (filtered) { 
    if (sec2 == "SI@FORTS" | sec2 == "EU@FORTS") {
      tabmod[grepl(substr(sec2, 1, 2), Security) & nchar(Security) == 4, Security := sec2]
    } else {
      tabmod[grepl(substr(sec2, 1, 4), Security), Security := sec2]
    }
  }
  if (filtered) {
    if (sec1 == "USD000UTSTOM@CETS") {
      tabmod[Security == "USD000000TOD", Security := sec1]
    } else if (sec1 == "EUR_RUB__TOM@CETS") {
      tabmod[Security == "EUR_RUB__TOD@CETS", Security := sec1]
    }
  }
  tabmod[Security == sec1, Volume := Volume*weights[1]]
  tabmod[Security == sec2, Volume := Volume*weights[2]]
print("weights")
  if (filtered == FALSE) {
    if (sec1 == "USD000UTSTOM@CETS") {
      tab1 <- tabmod[Security %in% c("USD000UTSTOM@CETS", "USD000000TOD", "USD000TODTOM"), ]
    } else if (sec1 == "EUR_RUB__TOM@CETS") {
      tab1 <- tabmod[Security %in% c("EUR_RUB__TOM@CETS", "EUR_RUB__TOD", "EUR000TODTOM")]
    }
    if (sec2 == "SI@FORTS" | sec2 == "EU@FORTS") {
      tab2 <- tabmod[grepl(substr(sec2, 1, 2), Security) & nchar(Security) == 4, ]
    } else {
      tab2 <- tabmod[grepl(substr(sec2, 1, 4), Security), ]
    }    
  } else {
    tab1 <- tabmod[Security == sec1, ]
    tab2 <- tabmod[Security == sec2, ]
  }
  tab1[, Volume := Volume*weights[1]]
  tab2[, Volume := Volume*weights[2]]
  tab1$Date <- as.Date(tab1$Time)
  tab2$Date <- as.Date(tab2$Time)

  if (commentpattern1 != "") {    
    tab1 <- tab1[grepl(commentpattern1, Comment), ]
  }
  if (commentpattern2 != "") {
    tab2 <- tab2[commentpattern2 == substr(Comment, 6, 6), ]
  }

  setkey(tab1, "Time")
  setkey(tab2, "Time")
  tab1[, id := 1:nrow(tab1)]
  tab2[, id := 1:nrow(tab2)]
  if (is.na(eps)) {
    return(list(deals = tab1, orders = tab2))
  } else {
    ###for stocks we need to reverse the data
    return(list(deals = tab2, orders = tab1))
  }
}
