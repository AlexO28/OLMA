#requires CalculatePlForShiny.R (basically requires Saule)
RecalculateProfitBySlippage <- function(datestart, dateend, port1, port2, commentpattern1 = "", commentpattern2 = "", slipvals, bank) {
  #we get the deals
  dat <- GetBrandNewDeals(port1, port2, datestart, dateend, "USD000UTSTOM@CETS", "SIU6@FORTS", commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
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
  print(summary(tabfut))
  res <- data.frame(slipval = numeric(length(slipvals)), prof = numeric(length(slipvals)), dohodnost = numeric(length(slipvals)))
#  profs <- SimpleProfitCalculator(tabdol, tabfut, datestart, dateend, noclose, tabquotes, swapres, bank)
  for (j in 1:length(slipvals)) {
    slipval <- slipvals[j]
    print(slipval)
    tabfut <- RecalculateSlippageInDeals(tabfut, slipval)
    profs <- SimpleProfitCalculator(tabdol, tabfut, datestart, dateend, noclose, tabquotes, swapres, bank)
    res[j, ] <- c(slipval, profs[1], profs[2])
  }
res
}
SimpleProfitCalculator <- function(tabdol, tabfut, datestart, dateend, noclose, tabquotes, swapinfo, bank) {
  hres <- AnalysisWarDeals(tabdol, tabfut, datestart, dateend, 1, NA, swapinfo$posstart, noclose = noclose)
  truepl <- hres$truepl + swapinfo$swapprofit

  if (nrow(tabquotes)>0) {
    posdol <- hres$finpos[1]
    possi <- hres$finpos[2]
    hres$plbyquotes <- 1000*ifelse(posdol>=0, tabquotes$dol.bid[nrow(tabquotes)]*abs(posdol), -tabquotes$dol.ask[nrow(tabquotes)]*abs(posdol)) +
                       ifelse(possi>=0, tabquotes$si.bid[nrow(tabquotes)]*abs(possi), -tabquotes$si.ask[nrow(tabquotes)]*abs(possi))
    hres$quotetime <- tabquotes$Time[nrow(tabquotes)]
    hres$stavka <- ifelse(possi>=0, tabquotes$si.bid[nrow(tabquotes)], tabquotes$si.ask[nrow(tabquotes)])/1000 -
                    ifelse(posdol>=0, tabquotes$dol.bid[nrow(tabquotes)], tabquotes$dol.ask[nrow(tabquotes)])
    truepl <- truepl + hres$plbyquotes
  }
  trueplAnn <- 365*100*truepl/((as.numeric(dateend) - as.numeric(datestart))*bank)   
c(truepl, trueplAnn)
}
RecalculateSlippageInDeals <- function(dealsfut, slipval) {
  dealsfut <- as.data.table(as.data.frame(dealsfut))
  dealsfut[, idealprice := as.numeric(substr(Comment, 1, 5))]
  dealsfut[, Price := idealprice + ifelse(Direction == "Sell", -slipval, slipval)]  
}
GetClassifiedSlippageInfo <- function(datestart, dateend, port1, port2, commentpattern1 = "", commentpattern2 = "", slipval, timelen) {
  dat <- GetBrandNewDeals(port1, port2, datestart, dateend, "USD000UTSTOM@CETS", "SIU6@FORTS", commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  tabdoltom <- dat$tom
  tabdoltod <- dat$tod
  tabfut <- dat$fut
  if (nrow(tabdoltod) > 0) {
    res <- BrandNewFullMatch(tabdoltod, tabdoltom, fullmode = TRUE)
    #swaptab <- res$res
    tabdol <- res$tab2 
  } else {
    tabdol <- tabdoltom
  }
  tabfut <- GetClosestDolDeal(tabdol, tabfut, timelen + 1)
  slipstrut <- CalculateSlippageInWar(tabfut)
  tabsi <- data.table(Time = tabfut$Time, dolid = tabfut$dolid, slippage = slipstrut$slippage)
  setkey(tabsi, "dolid")
  setkey(tabdol, "id")
  tabsi <- na.omit(tabsi[tabdol, ])
#list(fut = tabfut, slip = slipstrut, spot = tabdoltom)
tabsi[, timediff := as.numeric(Time - i.Time)]
print(summary(tabsi$timediff))
#first part
inds1 <- which(tabsi$timediff <= 1 & tabsi$slippage <= slipval)
perc1 <- nrow(tabsi[inds1, ])/nrow(tabsi)
vals1 <- tabsi$slippage[inds1]
tabsiun <- tabsi[-inds1, ]
#second part
perc2 <- nrow(tabsi[slippage >= 50, ])/nrow(tabsi)
#third part
perc3 <- nrow(tabsiun[slippage < 50 & timediff < timelen-1, ])/nrow(tabsi)
vals2 <- tabsiun$timediff[tabsiun$slippage < 50  & tabsiun$timediff < timelen-1]
#fourth part
perc4 <- nrow(tabsi[slippage < 50 & timediff >= timelen-1, ])/nrow(tabsi)
vals3 <- tabsi$slippage[tabsi$slippage < 50 & tabsi$timediff >= timelen-1]
list(perc1 = perc1, perc2 = perc2, perc3 = perc3, perc4 = perc4,
     vals1 = vals1, vals2 = vals2, vals3 = vals3, fullvals = tabsi$timediff)
}
GetClosestDolDeal <- function(tabdol, tabfut, len) {
  tabfut$dolid <- NA
  for (j in 1:nrow(tabfut)) {
    tabdolred <- tabdol[Time <= tabfut$Time[j] & Time >= tabfut$Time[j] - len, ]
    if (nrow(tabdolred) > 0) {
      resid <- tabdolred$id[nrow(tabdolred)]
    } else {
      tabdolred <- tabdol[Time <= tabfut$Time[j] + 1 & Time >= tabfut$Time[j] - len, ]
      resid <- tabdolred$id[1]
    }
    tabfut$dolid[j] <- resid
  }
tabfut
}
GetSlipStrutByTrader <- function(datestart, dateend, port1, port2, commentpattern1, commentpattern2) {
  deals <- GetDealsForMain(port1, port2, datestart, dateend, "USD000UTSTOM@CETS", "SIU6@FORTS", filtered = FALSE, 
                           commentpattern1 = commentpattern1, commentpattern2 = commentpattern2)
  slipstrut <- CalculateSlippageInWar(deals$orders)
slipstrut
}
GetConfIntForWMSlipStrut <- function(slipstrut, num) {
  slipstrut <- as.data.frame(slipstrut)
  GetConfIntForWMSlipStrut1Iter <- function(iter, slipstrut) {
    inds <- sample(nrow(slipstrut), size = nrow(slipstrut), replace = TRUE)
    slipnew <- slipstrut[inds, ]
    sum(slipnew$slippage*abs(slipnew$volume))/sum(abs(slipnew$volume))
  }
  vec <- sapply(1:num, "GetConfIntForWMSlipStrut1Iter", slipstrut = slipstrut)
#it is better to take 0.025 here
c(quantile(vec, 0.05), quantile(vec, 0.95))
}
InvestigateSlipStrut <- function(slipstrut) {
  weightmean <- sum(slipstrut$slippage*abs(slipstrut$volume))/sum(abs(slipstrut$volume))
  confintwm <- GetConfIntForWMSlipStrut(slipstrut, 10000)
  val1 <- nrow(slipstrut[slippage <= 3, ])/nrow(slipstrut)
  val2 <- nrow(slipstrut[slippage <= 15, ])/nrow(slipstrut)
c(weightmean, confintwm, val1, val2)
}
MakeSlippagePlotDay <- function(adate, times, tab) {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[Date == adate, ]
  plot(tab$Time, tab$Close, type = "l")
  times <- times[as.Date(times) == adate]
print(times)
print(length(times))
  if (length(times) > 0) {
    for (atime in times) {
      abline(v = as.POSIXct(atime), col = "red")
    }
  }
}
StudyVolatilityByDate1 <- function(adate, tab, times, aname, loclen = 300, globlen = 600) {
  tabred <- tab[Date == adate, ]
  timesred <- times[as.Date(times) == adate]
  volloc <- sapply(timesred, "EstimateVolatilityForTimeMoment1", tab = tabred, aname = aname, loclen = loclen, globlen = globlen)
}
GetSlippagePercent <- function(slipstrut, val, all = FALSE) {
  dates <- unique(slipstrut$Date)
  res <- data.frame(Date = Sys.Date(), Val = NA)
  if (all) {
    return(nrow(slipstrut[slippage <= val, ])/nrow(slipstrut))
  }
  for (adate in dates) {
    slipred <- slipstrut[slipstrut$Date == adate, ]
    perc <- nrow(slipred[slipred$slippage <= val,])/nrow(slipred)
    res <- rbind(res, data.frame(Date = adate, Val = perc))
  }
na.omit(res)
}
SlippageAnalysisBySteps <- function(slipstrut) {
  res <- data.frame(id = numeric(), val = numeric())
  for (j in 1:15) {
    res <- rbind(res, data.frame(id = j, val = nrow(slipstrut[slippage <= j])/nrow(slipstrut)))
  }
res
}
