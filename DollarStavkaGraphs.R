Sys.setenv(TZ = "GMT")
FillStavkaTab <- function(datestart, dateend, spotsec = "USD000UTSTOM@CETS", futsec = "SIH6@FORTS",
                          candletype = "1sVol50", mod = 1000, expinfo = NA, type = "spot",
                          storage.path = "D:\\") {
  if (candletype == "1mVol50" | candletype == "Data") {
    timelen <- 1
  } else {
    timelen <- 60
  }
  tabspot <- GetCleanData("spot", spotsec, datestart = datestart, dateend = dateend,
                          storage.path = storage.path, candletype = candletype, timelen = timelen)
  if (type == "fut") {
    tabfut <- GetCleanData("fut", "SI", datestart = datestart, dateend = dateend,
                         storage.path = storage.path, candletype = candletype, timelen = timelen, expinfo = expinfo)
  } else if (type == "spot") {
    tabfut <- GetCleanData("spot", futsec, datestart = datestart, dateend = dateend,
                           storage.path = storage.path, candletype = candletype, timelen = timelen, expinfo = expinfo)
  }
  tab <- InnerMergeDf(list(dol = tabspot, si = tabfut), "Time")
  tab[, stavkabid := (si.bid/mod - dol.bid)]
  tab[, stavkaask := (si.ask/mod - dol.ask)]
  tab[, stavka := (stavkabid +stavkaask)/2]
  tab[, Date := dol.date]
}
ModifyTheSwaps <- function(tab) {
  tab <- as.data.frame(tab)
  for (name in c("Bid", "Ask", "Close", "stavka")) {
    print(name)
    tab[tab$wday == 5, name] <- tab[tab$wday == 5, name]/3
    tab[tab$Date == as.Date("2015-02-13"), name] <- 0.75*tab[tab$Date == as.Date("2015-02-13"), name]
    tab[tab$Date == as.Date("2015-02-20"), name] <- 0.75*tab[tab$Date == as.Date("2015-02-20"), name]
    tab[tab$Date == as.Date("2015-03-06"), name] <- 0.75*tab[tab$Date == as.Date("2015-03-06"), name]
    tab[tab$Date == as.Date("2015-04-30"), name] <- tab[tab$Date == as.Date("2015-04-30"), name]/5
    tab[tab$Date == as.Date("2015-05-08"), name] <- 0.6*tab[tab$Date == as.Date("2015-05-08"), name]
    tab[tab$Date == as.Date("2015-05-22"), name] <- 0.75*tab[tab$Date == as.Date("2015-05-22"), name]
    tab[tab$Date == as.Date("2015-06-11"), name] <- tab[tab$Date == as.Date("2015-06-11"), name]/4
    tab[tab$Date == as.Date("2015-09-04"), name] <- 0.75*tab[tab$Date == as.Date("2015-09-04"), name]
    tab[tab$Date == as.Date("2015-10-09"), name] <- 0.75*tab[tab$Date == as.Date("2015-10-09"), name]
    tab[tab$Date == as.Date("2015-11-03"), name] <- tab[tab$Date == as.Date("2015-11-03"), name]/2
    tab[tab$Date == as.Date("2015-11-10"), name] <- tab[tab$Date == as.Date("2015-11-10"), name]/2
    tab[tab$Date == as.Date("2015-11-25"), name] <- tab[tab$Date == as.Date("2015-11-25"), name]/2
  }
tab[!(tab$Date %in% c(as.Date("2015-04-16"), as.Date("2015-02-17"), as.Date("2015-06-01"))), ]
}
expinfo <- data.frame(
  start = as.Date(c("2015-02-02", "2015-03-17", "2015-07-16", "2015-09-16", "2015-12-16",
                    "2016-03-16")),
  end = as.Date(c("2015-03-16", "2015-07-15", "2015-09-15", "2015-12-15", "2016-03-15", "2016-06-15")),
  shortname = c("H5", "M5", "U5", "Z5", "H6", "M6"),
  fullname = character(6)
)
#requires Comparison.R
GetFilesByTraderName <- function(trader, storage = "D:\\ValuteTraders\\") {
  if (trader == "HFT") {
    files <- data.frame(spot = "FirstHorseman\\dealsMB0177809279.csv", fut = "FirstHorseman\\dealsR0L0LL0.csv")
  } else if (trader == "kolya") {
    files <- data.frame(spot = "Kolya\\dealsMB0177810005.csv", fut = "Kolya\\dealsR0L0L16.csv")
  } else if (trader == "igor") {
    files <- data.frame(spot = "Igor\\dealsMB0177809359.csv", fut = "Igor\\dealsR0L0L13.csv")
  } else if (trader == "oleg") {
    files <- data.frame(spot = "SecondHorseman\\dealsMB0177808169.csv", fut = "SecondHorseman\\dealsR0L0L00.csv")
  } else if (trader == "ashot") {
    files <- data.frame(spot = "Ashot\\dealsMB0177809432.csv", fut = "Ashot\\dealsR0L0LL3.csv")
  } else if (trader == "kolya1") {
    files <- data.frame(spot = "Kolya\\dealsMB0177810421.csv", fut = "Kolya\\dealsR0L0L21.csv")
  } else if (trader == "kolya2") {
    files <- data.frame(spot = "Kolya\\dealsMB0177805974.csv", fut = "Kolya\\dealsR0L0L19.csv")
  } else if (trader == "kolya3") {
    files <- data.frame(spot = "Kolya\\dealsMB0177810488.csv", fut = "Kolya\\dealsR0L0LL4.csv")
  } else if (trader == "elvina") {
    files <- data.frame(spot = "Elvina\\dealsL01+00000F00.csv",
                        fut = "Elvina\\dealsR0L0100.csv")
  } else if (trader == "shabanov") {
    files <- data.frame(spot = "Shabanov\\dealsMB0177810693.csv",
                        fut = "Shabanov\\dealsR0L0LL6.csv")
  } else if (trader == "falcons") {
    #files <- data.frame(spot = "SaratovFalcon\\dealsMB0177810502.csv",
    #                    fut = "SaratovFalcon\\dealsR0L0L17.csv")
    files <- data.frame(spot = "SaratovFalcon\\dealsMB0177810997.csv",
                        fut = "SaratovFalcon\\dealsR0L0L22.csv")
  } else if (trader == "sergey") {
    files <- data.frame(spot = "dealsL11+00000F08.csv",
                        fut = "dealsR0L0LL2.csv")
  } else if (trader == "kolya4") {
    files <- data.frame(spot = "Kolya\\dealsMB0177810005.csv",
                        fut = "Kolya\\dealsR0L0L16.csv")
  } else {
     return(NA)
  }
data.frame(spot = paste0(storage, files$spot), fut = paste0(storage, files$fut))
}
GetStavkaDataForTrader <- function(trader, storage = "D:\\ValuteTraders\\",
                                   curfutname = "SIZ5@FORTS", inplotstandard = FALSE,
                                   curspotname = "SBER@TQBR", fromrobot = FALSE) {
  print(trader)
  print(storage)
  if ((trader != "me") & (trader != 'simme')) {
    files <- GetFilesByTraderName(trader, storage)
    print(files)
    if (trader != "elvina" & trader != 'sergey') {
      return(  GetStavkaDeals(files$spot, files$fut, "USD000UTSTOM@CETS", curfutname, inplotstandard = inplotstandard))
    } else if (trader == "elvina"){
      print("elvina mode")
      return(  GetStavkaDeals(files$fut, files$spot, curfutname, curspotname, inplotstandard = inplotstandard))
    } else if (trader == "sergey") {
      print("sergey mode")
      if (!fromrobot) {
        return(GetStavkaDeals(files$fut, files$spot, curfutname, curspotname, inplotstandard = inplotstandard, fromrobot = FALSE))
      } else {
        return(GetStavkaDeals("D:\\ValuteTraders\\Sergey\\deals.txt", "D:\\ValuteTraders\\Sergey\\deals.txt", curfutname, curspotname, inplotstandard = inplotstandard, fromrobot = TRUE))
      }
    }
  } else if (trader == 'me') {
    ###requires war_script.R
    deals <- GetWarDeals(paste0(storage, "deals.txt"))
    deals <- deals[Date >= as.Date("2016-05-19"), ]
    print(names(deals))
    print(summary(deals))
    mtab <- AnalyzeWarDeals(deals, fullanalysis = FALSE, war = TRUE, curfutname = curfutname)
  } else {
    deals <- fread(paste0(storage), select = c(1, 3, 6, 5, 4))
    deals[, Time := fastPOSIXct(Time, tz = "GMT")]
    deals[, Date := as.Date(Time)]
    mtab <- AnalyzeWarDeals(deals, fullanalysis = FALSE, war = FALSE, curfutname = curfutname)
  }
mtab
}
GetStavkaDeals <- function(filename1, filename2, secname1, secname2, inplotstandard = FALSE, fromrobot = FALSE) {
  if (secname2 == "SBER@TQBR" | secname2 == "GAZP@TQBR" |
      secname2 == "LKOH@TQBR" | secname2 == "VTBR@TQBR") {
    weights <- c(100, 10)
    eps <- 50
    if (secname2 == "LKOH@TQBR") {
      mod <- 10
    } else if (secname2 == "VTBR@TQBR") {
      mod <- 100000
    } else {
      mod <- 100
    }
  } else {
    weights <- NA
    eps <- NA
    mod <- 1000
  }
  print("readytostart")
  if (!fromrobot) {
    if (is.na(weights)) {
      dat <- GetData(filename1, filename2, secname1, secname2)
    } else {
      dat <- GetData(filename1, filename2, secname1, secname2, weights = weights)
    }
  } else {
    fulldeals <- fread(filename1,  dec = ".")
    fulldeals$Time <- NULL
    fulldeals$TradeTime <- NULL
    fulldeals[, Time := as.POSIXct(LocalTime, format = "%d-%m-%Y %H:%M:%OS")]
    fulldeals$LocalTime <- NULL
    fulldeals$Side <- fulldeals$Direction
    fulldeals$Price <- as.numeric(fulldeals$Price)
    deals1 <- fulldeals[Security == secname1, ]
    deals2 <- fulldeals[Security == secname2, ]
    weights2 <- weights
    if (is.na(weights2)) {weights2 <- c(1, 1)}
    print(paste0("weight is", weights2[1]))
    print(secname1)
    deals1[, ":="(Volume = weights2[1]*Volume*ifelse(Side == "Buy", 1, -1), Date = as.Date(Time))]
    print(paste0("weight is", weights2[2]))
    print(secname2)
    deals2[, ":="(Volume = weights2[2]*Volume*ifelse(Side == "Buy", 1, -1), Date = as.Date(Time))]
    dates <- intersect(unique(deals1$Date), unique(deals2$Date))
    deals1 <- deals1[Date %in% dates, ]
    deals2 <- deals2[Date %in% dates, ]
    deals1[, id := 1:nrow(deals1)]
    deals2[, id := 1:nrow(deals2)]
    dat <- list(deals = deals1, orders = deals2)
  }
  print(str(dat))
  if (is.na(eps)) {
    print("no epsilon")
    deals <- HedgeComparison2(dat)
  } else {
    deals <- HedgeComparison2(dat, eps = eps)
  }
  print("HedgeComparison ended")
  mtab <- GetHedgeDiff(dat, deals)
  print(mtab$val)
  mtab <- mtab$tab
  print(head(mtab))
  print(mod)
  if ((secname2 == "SBER@TQBR") | (secname2 == "GAZP@TQBR") |
      (secname2 == "LKOH@TQBR") | (secname2 == "VTBR@TQBR")) {
    res <- data.frame(Date = mtab$Date, Time = mtab$Time, spotprice = mtab$price, futprice = mtab$Price,
                      Side = mtab$side, Volume = mtab$volume, diff = mtab$Price - mod*mtab$price)
  } else {
    res <- data.frame(Date = mtab$Date, Time = mtab$Time, spotprice = mtab$Price, futprice = mtab$price,
            Side = mtab$Side, Volume = mtab$Volume, diff = mtab$price - mod*mtab$Price)
  }
  res$pos <- cumsum(-res$Volume)
  if (!inplotstandard) {return(res)} else {
    if (secname2 != "SBER@TQBR" & secname2 != "GAZP@TQBR" &
        secname2 != "LKOH@TQBR" & secname2 != "VTBR@TQBR") {
      res <- data.table(usd.Time = mtab$Time, usd.Security = mtab$Security, usd.Volume = mtab$Volume,
                      usd.Price = mtab$Price, usd.Direction = mtab$Side, usd.Date = mtab$Date,
                      usd.hedgeid = mtab$hedgeid, usd.hedgeidmin = mtab$hedgeidmin,
                      si.Time = mtab$time, si.Security = mtab$security, si.Volume = mtab$volume,
                      si.Price = mtab$price/mod, si.Direction = mtab$side, si.Date = mtab$date,
                      si.id = mtab$id, diff = res$diff, stavka = res$futprice/mod - res$spotprice,
                      deltatime = as.numeric(mtab$time) - as.numeric(mtab$Time))
    } else {
      res <- data.table(usd.Time = mtab$time, usd.Security = mtab$security, usd.Volume = mtab$volume,
                        usd.Price = mtab$price, usd.Direction = mtab$side, usd.Date = mtab$date,
                        usd.hedgeid = mtab$hedgeid, usd.hedgeidmin = mtab$hedgeidmin,
                        si.Time = mtab$Time, si.Security = mtab$Security, si.Volume = mtab$Volume,
                        si.Price = mtab$Price/mod, si.Direction = mtab$Side, si.Date = mtab$Date,
                        si.id = mtab$id, diff = res$diff, stavka = res$futprice/mod - res$spotprice,
                        deltatime = as.numeric(mtab$time) - as.numeric(mtab$Time))
    }
  }
res
}
GetStatInfoForTraderDeals <- function(stavdat) {
  stavdat <- as.data.table(stavdat)
  GetLast <- function(x) {x[length(x)]}
  stavdat[, list(numdeals = length(Volume), rpl = sum(Volume), pos = GetLast(pos)), by = Date]
}
DolInit <- function(datestart, dateend, candletype = "1m", currency = "dol") {
  if (currency == "dol") {
    futname <- "SI"
    spotname <- "USD000UTSTOM@CETS"
    swapname <- "USD000TODTOM@CETS"
  } else {
    futname <- "EU"
    spotname <- "EUR_RUB__TOM@CETS"
    swapname <- "EUR000TODTOM@CETS"
  }
  expinfo$fullname <- paste0(futname, expinfo$shortname, "@FORTS")
  tabSI <- as.data.frame(LoadBpVarInstrs(storage = "D:", candletype = candletype, expinfo = expinfo))
  tabUSD <- as.data.frame(LoadBp(spotname, as.Date(datestart), as.Date(dateend), candle.type = candletype, storage.path = "D:"))
  tabswap <- as.data.frame(LoadBp(swapname, as.Date(datestart), as.Date(dateend), candle.type = candletype, storage.path = "D:"))

  tabswap$Close <- (tabswap$Bid + tabswap$Ask)/2
#tabswap$Close <- 1000*tabswap$Close
  tabswap$wday <- as.POSIXlt(tabswap$Date)$wday
  tabswap <- ModifyTheSwaps(tabswap)
  tab <- InnerMergeDf(list(si = tabSI, usd = tabUSD, swap = tabswap), "Time")
  tab <- na.omit(tab)
  names(tab) <- tolower(names(tab))
  tab$Time <- tab$si.time
  tab <- MainInitialization(tab, "Time")
  mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"), tz = "GMT")
  tab <- tab[mask>0, ]
  tab$usd.bid <- as.numeric(tab$usd.bid)
  tab$usd.ask <- as.numeric(tab$usd.ask)
  if (candletype == "1m") {multip <- 1} else if (candletype == "1s") {multip <- 60} else {multip <- NA}
  ind1 <- Purify2(tab, len = 15*multip, strname = "si.")
  ind2 <- Purify2(tab, len = 15*multip, strname = "usd.")
  if (length(ind1)>0 | length(ind2)>0) {tab2 <- tab[-union(ind1, ind2), ]} else {tab2 <- tab}
  tab2$usd.bid <- tab2$usd.bid
  tab2$usd.ask <- tab2$usd.ask
  tab2$usd.close <- (tab2$usd.bid + tab2$usd.ask)/2
  tab2$si.bid <- tab2$si.bid/1000
  tab2$si.ask <- tab2$si.ask/1000
  tab2$si.close <- (tab2$si.bid + tab2$si.ask)/2
  tab2$diff <- tab2$si.close - tab2$usd.close
  print(summary(tab2))
  tab3 <- KillQuantiles(tab2, 0.05, "diff", 15)
  print(summary(tab3))
  htab <- aggregate(tab3$si.bid, list(tab3$date), length)
  print(htab)
  days <- htab[htab[, 2] >= 200, 1]
  tab4 <- tab3[tab3$date %in% days, ]
  tab4$stavkabid <- tab4$si.bid - tab4$usd.ask
  tab4$stavkaask <- tab4$si.ask - tab4$usd.bid
tab4
}
GetDevStatDat <- function(tab, control) {
  tab <- as.data.table(tab)
  fun <- function(x, y) {
    quantile(y-x, 0.9)
  }
  fun2 <- function(x, y) {
    z <- (x+y)/2
    quantile(z, 0.9) - quantile(z, 0.1)
  }
  if (control == 2) {
    return(tab[, list(dev = fun2(stavkabid, stavkaask)), by = "date"])
  } else {
    return(tab[, list(dev = fun(stavkabid, stavkaask)), by = "date"])
  }
}
GetProfStatDat <- function(tab) {
  tab <- as.data.table(tab)
  tabsell <- tab[Side == "Sell", ]
  tabbuy <- tab[Side == "Buy", ]

  fun <- function(price, volume) {sum(price*abs(volume))/sum(abs(volume))}
  mtabsell <- tabsell[, list(Diff = fun(diff, Volume)), by = "Date"]
  mtabbuy <- tabbuy[, list(diff = fun(diff, Volume)), by = "Date"]
  #data.table(Date = mtabsell$Date, diff = mtabsell$Diff - mtabbuy$Diff)
  setkey(mtabsell, "Date")
  setkey(mtabbuy, "Date")
  mtab <- mtabbuy[mtabsell, nomatch = 0]
  mtab[, prof := -Diff + diff]
}
PlotOfFive <- function(tab1, tab2, tab3, tab4, tab5, col1, col2) {
  mm <- rbind(c(1, 1), c(2, 3), c(4, 5))
  layout(mm)
  plot(tab1[, col1, with = FALSE][[1]], tab1[, col2, with = FALSE][[1]], ylab = col2, xlab = col1)
  plot(tab2[, col1, with = FALSE][[1]], tab2[, col2, with = FALSE][[1]], ylab = col2, xlab = col1)
  plot(tab3[, col1, with = FALSE][[1]], tab3[, col2, with = FALSE][[1]], ylab = col2, xlab = col1)
  plot(tab4[, col1, with = FALSE][[1]], tab4[, col2, with = FALSE][[1]], ylab = col2, xlab = col1)
  plot(tab5[, col1, with = FALSE][[1]], tab5[, col2, with = FALSE][[1]], ylab = col2, xlab = col1)
}
GetProfsInfo <- function(devtab1, devtab2, stavdat) {
  profhft <- GetProfStatDat(stavdat)
  setkey(profhft, "Date")
  profhft1 <- devtab1[profhft, nomatch = 0]
  profhft2 <- devtab2[profhft, nomatch = 0]
  list(prof1 = profhft1, prof2 = profhft2)
}
GetCommonPlotForTrader <- function(tab, trader) {
  layout(matrix(1:2, nrow = 2))
  plot(tab$Time, (tab$stavkabid + tab$stavkaask)/2)
  lines(tab$Time, tab$swap.close*(tab$daystillexpir-2), col = "blue")
  lines(tab$Time, tab$swap.close*(tab$daystillexpir), col = "red")
  plot(tab$Time, rep(mean(stavdat$pos), nrow(tab)), type = "n", ylim = c(min(stavdat$pos), max(stavdat$pos)))
  lines(stavdat$Time, stavdat$pos)
  abline(h = 0, col = "red")
}
GetSimPlots <- function(tab, tabswap, curfutname, datestart, dateend, report = "C:\\ArbitrazhyorSimulation\\Reports\\2016-03-30- 18-10-57\\", id = 0) {
  filename <- DefineFileById(id)
  GetPlotsForMeComparison(tab, tabswap, trader = "simme", curfutname, datestart, dateend,
                          report = paste0(report, "IterationsResult\\", filename), name = paste0("sim_", id))
}
#in certain regime requires Accumulator.R
GetPlotsForMeComparison <- function(tab, tabswap, trader="me", curfutname, datestart, dateend, ashotrequest = FALSE, curspotname = "SBER@TQBR", report = "C:\\ArbitrazhyorSimulation\\Reports\\2016-03-30- 18-10-57\\IterationsResult\\00000trades.csv",
                                    name = NA, variousvolumes = FALSE, accumvol = NA,
                                    onlymaingraph = FALSE, withpositiongraph = FALSE) {
  print(report)
  if (is.na(name)) {name <- trader}
  if (trader == "me") {
    stavdat <- GetStavkaDataForTrader("me", storage = "D:\\", curfutname = curfutname)
  } else if (trader == "sergey") {
    stavdat <- GetStavkaDataForTrader(trader, curfutname = curfutname, inplotstandard = TRUE, curspotname = curspotname,
                                      storage = "D:\\Data\\", fromrobot = TRUE)
    print(summary(stavdat))
  } else if (trader == "simme") {
    stavdat <- GetStavkaDataForTrader("simme", storage = report, curfutname = curfutname)
  } else {
    stavdat <- GetStavkaDataForTrader(trader, curfutname = curfutname, inplotstandard = TRUE, curspotname = curspotname)
  }
  stavdatprev <- stavdat
  if (!is.na(accumvol)) {
    stavdat <- AccumulateTrades(stavdat, accumvol)
    if (nrow(stavdat) == 0) {
      stop("not enough volume is accumulated")
    }
    print(summary(stavdat))
  }
  tab[, stavka := (stavkabid + stavkaask)/2]
  stavdat <- stavdat[si.Date >= datestart & si.Date <= dateend, ]
  if (!is.na(tabswap)) {
    dates <- unique(tabswap$date)
    for (j in 1:nrow(tabswap)) {
      closeprice <- tabswap$Close[j]
      adate <- tabswap$Date[j]
      tab[si.date < adate, stavka := stavka - closeprice]
      stavdat[si.Date < adate, stavka := stavka - closeprice]
    }
  }
  tab[, id := 1:nrow(tab)]
  setkey(tab, "Time")
  setkey(stavdat, "si.Time")
  ctab <- tab[stavdat, roll = TRUE, mult = "first"]
  ctabprev <- tab[stavdatprev, roll = TRUE, mult = "first"]
  print(summary(ctab$stavka))
#stop()
  jpeg(paste0("Plot", name, curspotname, ".jpeg"), width = 1900, height = 1200)
  if (ashotrequest) {layout(matrix(1:2, nrow = 2))}
  if (withpositiongraph) {
    layout(matrix(1:2, nrow = 2))
  } else {
    layout(1)
  }
  plot(tab$id, tab$stavka, ylim = c(min(stavdatprev$stavka, tab$stavka), max(stavdatprev$stavka, tab$stavka)), type = "l", main = trader, ylab = "basis")
  # plot(tab$id, tab$stavka, type = "l", main = trader)
  #I used it by some reason (mainly for Sergey and various volumes)
  #ctabvols <- abs(ctab$usd.Volume)
  ctabvols <- abs(ctab$si.Volume)
  quant0 <- min(ctabvols)
  quant4 <- max(ctabvols)
  if (variousvolumes) {
    for (k in 1:nrow(ctab)) {
      if (ctab$si.Direction[k] == "Buy") {
        points(ctab$id[k], ctab$i.stavka[k], col = "green", bg = "green", pch = 24, cex = 1 + 4*abs(ctab$si.Volume[k] - quant0)/(quant4 - quant0))
      } else {
        points(ctab$id[k], ctab$i.stavka[k], col = "red", bg = "red", pch = 25, cex = 1 + 4*abs(ctab$si.Volume[k] - quant0)/(quant4 - quant0))
      }
    }
  } else {
    points(ctab$id[ctab$si.Direction == "Buy"], ctab$i.stavka[ctab$si.Direction == "Buy"], col = "green", bg = "green", pch = 24, cex = 1)
    points(ctab$id[ctab$si.Direction == "Sell"], ctab$i.stavka[ctab$si.Direction == "Sell"], col = "red", bg = "red", pch = 25, cex = 1)
  }
  if (withpositiongraph) {
    #cumposes <- cumsum(ifelse(ctabprev$si.Direction == "Buy", ctabprev$si.Volume, - ctabprev$si.Volume))
    cumposes <- cumsum(ctabprev$si.Volume)
    print(c(-max(abs(cumposes)), max(abs(cumposes))))
    datetab <- aggregate(tab$id, list(tab$Date), function(x) {x[1]})
    for (k in 1:nrow(datetab)) {
      abline(v = datetab[k, 2])
    }
    plot(tab$id, type = "n", ylab = "position", ylim = c(-max(abs(cumposes)), max(abs(cumposes))))
    lines(c(1, ctabprev$id), c(0, cumposes), col = "red")
    lines(c(1, ctab$id), c(0, cumsum(ctab$si.Volume)), col = "green")
    abline(h = 0)
    if (!is.na(accumvol)) {
      for (k in 1:floor(max(abs(cumposes)/accumvol))) {
        print(k)
        abline(h = k*accumvol)
        abline(h = -k*accumvol)
      }
    }
  }
  if (ashotrequest) {
    plot(tab$id, tab$si.close, ylab = "futures")
  }
  dev.off()
  if (!onlymaingraph) {
    ctabsell <- ctab[si.Direction == "Sell", ]
    ctabbuy <- ctab[si.Direction == "Buy", ]
    print("ratio of good sell-deals")
    rel <- signif(100*nrow(ctabsell[stavka - i.stavka<=0,])/nrow(ctabsell), 4)
    print(rel)
    jpeg(paste0("HistSell", name, curspotname, ".jpeg"), width = 1900, height = 1200)
    truehist(ctabsell$stavka - ctabsell$i.stavka, ylab = "Market minus trader sell-deal",
             main = paste0(trader,": ", rel, " good deals"))
    dev.off()
    print("ratio of good buy-deals")
    rel <- signif(100*nrow(ctabbuy[stavka - i.stavka>=0,])/nrow(ctabbuy), 4)
    print(rel)
    jpeg(paste0("HistBuy", name, curspotname, ".jpeg"), width = 1900, height = 1200)
    truehist(ctabbuy$stavka - ctabbuy$i.stavka, ylab = "Market minus trader buy-deal",
             main = paste0(trader,": ", rel, " good deals"))
    dev.off()
    return(list(tab = tab, stavdat = stavdat, ctab = ctab, ctabprev = ctabprev))
  }
}
PlotPositionByDat <- function(dat, tradername) {
  tab <- dat$tab
  ctab <- dat$ctab
  ctabprev <- dat$ctabprev
  cumposes <- cumsum(ctabprev$si.Volume)
  print(c(-max(abs(cumposes)), max(abs(cumposes))))
  plot(tab$id, type = "n", ylab = "position", main = tradername, ylim = c(-max(abs(cumposes)), max(abs(cumposes))))
  lines(c(1, ctabprev$id), c(0, cumposes), col = "red")
  lines(c(1, ctab$id), c(0, cumsum(ctab$si.Volume)), col = "green")
  abline(h = 0)
  datetab <- aggregate(tab$id, list(tab$Date), function(x) {x[1]})
  for (k in 1:nrow(datetab)) {
    abline(v = datetab[k, 2])
  }
}
GetHistForPlot <- function(tab) {
  tab <- tab$ctab
  tabred <- tab[usd.Direction == "Buy", ]
  rel <- nrow(tabred[tabred$i.stavka - tabred$stavka >= 0, ])/nrow(tabred)
  print(rel)
  truehist(tabred$i.stavka - tabred$stavka, xlab = "продажа ставки минус рынок")
}
GetDayPlotsForMeComparison <- function(tab) {
  stavdat <- GetStavkaDataForTrader("me", storage = "D:\\")
  stavdatsim <- GetStavkaDataForTrader("simme", storage = "C:\\ArbitrazhyorSimulation\\Reports\\2016-03-27- 13-56-55\\")
  dates <- unique(tab$si.date)
  print(dates)
  dates <- dates[dates %in% stavdat$si.Date]
  print(dates)
  for (adate in dates) {
    adate <- as.Date(adate)
    print(adate)
    stavdatred <- stavdat[stavdat$si.Date == adate, ]
    stavdatsimred <- stavdatsim[stavdatsim$si.Date == adate, ]
    tabred <- tab[tab$si.date == adate, ]
    jpeg(paste0("combo", as.character(adate), ".jpeg"), width = 1900, height = 1200)
    layout(matrix(1:2, nrow = 2))
    plot(tabred$Time, (tabred$stavkabid + tabred$stavkaask)/2,
         ylim = c(min(tabred$stavkabid), max(tabred$stavkaask)), type = "l")
    points(stavdatred$si.Time[stavdatred$si.Direction == "Buy"], stavdatred$stavka[stavdatred$si.Direction == "Buy"], col = "green", bg = "green", cex = 2, pch = 24)
    points(stavdatred$si.Time[stavdatred$si.Direction == "Sell"], stavdatred$stavka[stavdatred$si.Direction == "Sell"], col = "red", bg = "red", cex = 2, pch = 25)
    plot(tabred$Time, (tabred$stavkabid + tabred$stavkaask)/2,
         ylim = c(min(tabred$stavkabid), max(tabred$stavkaask)), type = "l")
    points(stavdatsimred$si.Time[stavdatsimred$si.Direction == "Buy"], stavdatsimred$stavka[stavdatsimred$si.Direction == "Buy"], col = "green", bg = "green", cex = 2, pch = 24)
    points(stavdatsimred$si.Time[stavdatsimred$si.Direction == "Sell"], stavdatsimred$stavka[stavdatsimred$si.Direction == "Sell"], col = "red", bg = "red", cex = 2, pch = 25)
    dev.off()
  }

}
GetDayPlotsForTrader <- function(tab, trader) {
  if ((trader != 'me') & (trader != 'simme')) {
    stavdat <- GetStavkaDataForTrader(trader)
  } else if (trader == "me"){
    stavdat <- GetStavkaDataForTrader(trader, storage = "D:\\")
  } else {
    stavdat <- GetStavkaDataForTrader(trader, storage = "C:\\ArbitrazhyorSimulation\\Reports\\Real\\")
  }
  dates <- unique(tab$si.date)
  print(dates)
  dates <- dates[dates %in% stavdat$si.Date]
  print(dates)
  for (adate in dates) {
    adate <- as.Date(adate)
    print(adate)
    stavdatred <- stavdat[stavdat$si.Date == adate, ]
    tabred <- tab[tab$si.date == adate, ]
    jpeg(paste0(trader, as.character(adate), ".jpeg"), width = 1900, height = 1200)
 #   layout(matrix(1:2, nrow = 2))
   # print(c(min(tabred$stavkabid+tabred$stavkaask)*0.25, 0.5*max(tabred$stavkabid + tabred$stavkaask)))
    print(min(tabred$stavkabid), max(tabred$stavkaask))
    print(summary(stavdatred$stavka))
    print(summary(stavdatred))
    plot(tabred$Time, (tabred$stavkabid + tabred$stavkaask)/2,
         #ylim = c(min((tabred$stavkabid+tabred$stavkask)/2, tabred$swap.close*tabred$daystillexpir), max(tabred$stavkaask, tabred$swap.close*tabred$daystillexpir)
         #     ylim = c(min(tabred$stavkabid+tabred$stavkaask)*0.25, 0.5*max(tabred$stavkabid + tabred$stavkaask)),
         ylim = c(min(tabred$stavkabid), max(tabred$stavkaask)), type = "l")
    points(stavdatred$si.Time[stavdatred$si.Direction == "Buy"], stavdatred$stavka[stavdatred$si.Direction == "Buy"], col = "green", cex = 2, pch = 24)
    points(stavdatred$si.Time[stavdatred$si.Direction == "Sell"], stavdatred$stavka[stavdatred$si.Direction == "Sell"], col = "red", cex = 2, pch = 25)
  #  lines(tabred$Time, tabred$swap.close*(tabred$daystillexpir-2), col = "blue")
  #  lines(tabred$Time, tabred$swap.close*(tabred$daystillexpir), col = "red")
#    plot(tabred$Time, rep(mean(stavdatred$pos), nrow(tabred)), type = "n", ylim = c(min(stavdatred$pos), max(stavdatred$pos)))
#    lines(stavdatred$Time, stavdatred$pos)
#    abline(h = 0, col = "red")
    dev.off()
  }
}
PlotStavkaSimple <- function(tab) {
  vec <- tab$swap.close*tab$daystillexpir
  plot(tab$time, tab$diff, type = "l", xlab = "????", ylab = "???????? ??????",
       ylim = c(min(vec, tab$diff), max(vec, tab$diff)))
  lines(tab$time, vec, col = "red")
  legend("topright", col = c("black", "red"), lty = 1, legend = c("F-S", "mean swap"))
#data.frame(tab$date, tab$diff-vec)
}
PlotAnnualizedStavkaSimple <- function(tab) {
  vec1 <- 100*365*tab$diff/(tab$usd.close*tab$daystillexpir)
  vec2 <- 100*365*tab$swap.close/tab$usd.close
  plot(tab$time, vec1, type = "l", xlab = "????", ylab = "????????????????? ???????? ??????",
       ylim = c(min(vec1, vec2), max(vec1, vec2)))
  lines(tab$time, vec2, col = "red")
}
PlotAnnualizedStavkaGlued <- function(tab) {
  vec1 <- 100*365*tab$diff/(tab$usd.close*tab$daystillexpir)
  vec2 <- 100*365*tab$swap.close/tab$usd.close
  plot(1:nrow(tab), vec1, type = "l", xlab = "????", ylab = "????????????????? ???????? ??????",
       ylim = c(min(vec1, vec2), max(vec1, vec2)))
  lines(1:nrow(tab), vec2, col = "red")
}
PlotStavkaGlued <- function(tab) {
  vec <- tab$swap.close*tab$daystillexpir
  plot(1:nrow(tab), tab$diff, type = "l", xlab = "????", ylab = "???????? ??????",
       ylim = c(min(vec, tab$diff), max(vec, tab$diff)))
  lines(1:nrow(tab), vec, col = "red")
  legend("topright", col = c("black", "red"), lty = 1, legend = c("F-S", "mean swap"))
}

PlotGraph1Alt <- function(tab) {
  mtab <- aggregate(tab$diff, list(tab$date), mean)
  mtab2 <- aggregate(tab$swap.close*tab$daystillexpir, list(tab$date), mean)

  print(summary(mtab))
  print(summary(mtab2))

 # plot(mtab[, 1], type = "n", ylim = c(min(mtab[, 2], mtab2[, 2]), max(mtab[, 2], mtab2[, 2])))
  plot(mtab[, 1], mtab[, 2], type = "l", xlab = "????", ylab = "???????? ??????",
       ylim = c(min(mtab[,2], mtab2[,2]), max(mtab[,2], mtab2[,2])))
  lines(mtab[, 1], mtab2[, 2], col = "red")
  legend("topright", col = c("black", "red"), lty = 1, legend = c("F-S", "mean swap"))
list(mtab = mtab, mtab2 = mtab2)
}
PlotGraph2Alt <- function(tab) {
  tab$v1 <- 100*365*tab$diff/(tab$usd.close*tab$daystillexpir)
  tab$v2 <- 100*365*tab$swap.close/tab$usd.close
  mtab <- aggregate(100*365*tab$diff/(tab$usd.close*tab$daystillexpir), list(tab$date), mean)
  mtab2 <- aggregate(100*365*tab$swap.close/tab$usd.close, list(tab$date), mean)

  print(summary(mtab))
  print(summary(mtab2))

  # plot(mtab[, 1], type = "n", ylim = c(min(mtab[, 2], mtab2[, 2]), max(mtab[, 2], mtab2[, 2])))
  plot(mtab[, 1], mtab[, 2], type = "l", xlab = "????", ylab = "???????? ????????????????? ??????",
       ylim = c(min(mtab[,2], mtab2[,2]), max(mtab[,2], mtab2[,2])))
  lines(mtab[, 1], mtab2[, 2], col = "red")
  legend("topright", col = c("black", "red"), lty = 1, legend = c("annualized F-S", "annualized mean swap"))

}
#swap/(365*usd.close)

PlotGraph1 <- function(tab) {
  boxplot(tab4$diff ~ tab4$date, ylab = "?????? F-S")
}
PlotGraph2 <- function(tab) {
#  boxplot(I(tab4$diff*3.65/(tab4$daystillexpir)) ~ tab4$date, ylab = "Graph of F-S")
#  boxplot(I(tab4$diff*tab4$daystillexpir/(3.65*tab4$usd.close)) ~ tab4$date, ylab = "????????????????? ??????")
#100*365*(tab$mix.close - tab$mixind.close)/(tab$mixind.close*tab$daystillexpir)
boxplot(I(100*365*tab$diff/(tab$usd.close*tab$daystillexpir)) ~ tab4$date, ylab = "????????????????? ??????")
}
PlotGraph3 <- function(tab) {
  fun <- function(vec) {
    quantile(vec, 0.9) - quantile(vec, 0.1)
  }
  fun2 <- function(date0, tab, num) {
    print(date0)
    tabred <- tab[tab$date <= as.Date(date0) & tab$date >= as.Date(date0) - num, ]
    fun(tabred$diff)
  }
  mtab <- aggregate(tab$diff, list(tab$date), fun)
  dates <- unique(tab$date)
#  mtab <- sapply(dates, FUN = fun2, tab = tab4, num = 4)
  print(summary(mtab))
#  plot(dates, mtab, type = "l", ylab = "????????? ?????? ?????????", xlab = "????")
  plot(dates, mtab[, 2], type = "l", ylab = "??????? ?????? ?????????", xlab = "????")
}
PlotGraph3Alt <- function(tab) {
  print(nrow(tab))
  tab <- tab[tab$daystillexpir >= 21, ]
  print(nrow(tab))
  fun <- function(vec) {
    quantile(vec, 0.9) - quantile(vec, 0.1)
  }
  fun2 <- function(date0, tab, num) {
    print(date0)
    #tabred <- tab[tab$date <= as.Date(date0) & tab$date >= as.Date(date0) - 10, ]
    tabred <- tab[tab$date >= as.Date(date0), ]
    tabred$diff <- 100*365*tabred$diff/(tabred$usd.close*tabred$daystillexpir)
    fun(tabred$diff)
    #max(tabred$diff) - min(tabred$diff)
  }
  dates <- unique(tab$date)
#  mtab <- aggregate(100*365*tab$diff/(tab$usd.close*tab$daystillexpir), list(tab$date), fun)
  mtab <- sapply(dates, FUN = fun2, tab = tab, num = 0)
###  mtab2 <- aggregate(tab$daystillexpir*tab$usd.close, list(tab4$date), mean)
#  print(summary(mtab[,2]))
#  vec <- 100*365*mtab[,2]/mtab2[, 2]
  print(summary(vec))
  #plot(mtab[,1], mtab[,2], type = "l", ylab = "????????????????? ??????? ?????? ?????????", xlab = "????")
  #plot(dates, mtab, type = "l", ylab = "??????? ?????? ????????? ????????????????? ?????? (????.)", xlab = "????")
  plot(dates, mtab, type = "l", ylab = "???", xlab = "????")
}
PlotGraph4 <- function(stavkadeals) {
  stavkasell <- stavkadeals[stavkadeals$Side == "Sell", ]
  stavkabuy <- stavkadeals[stavkadeals$Side == "Buy", ]
  dates <- unique(as.Date(stavkadeals$Date))
  fun <- function(val, tab) {
    tabred <- tab[tab$Date == val, ]
    sum(abs(tabred$Volume)*tabred$diff)/sum(abs(tabred$Volume))
  }
  buyvec <- sapply(dates, fun, tab=stavkasell)
  sellvec <- sapply(dates, fun, tab=stavkabuy)
  vec <- sellvec - buyvec
  plot(dates, vec, ylab = "???????? ???????????????? ??? ?? ???????", type = "l")
}
PlotGraph5 <- function(tab4) {
  tab4$stavkaask <- tab4$si.ask - tab4$usd.bid
  tab4$stavkabid <- tab4$si.bid - tab4$usd.ask
  tab4$spread <- tab4$stavkaask - tab4$stavkabid
  htab <- aggregate(tab4$spread, list(tab4$date), function(x) {sd(x)/mean(x)})
  plot(htab[,1], htab[,2], xlab = "????", ylab = "????????????? ?????? ??????/????? ??????", type = "l")
}

func <- function(vec) {
  as.numeric(quantile(vec, 0.9)) - as.numeric(quantile(vec, 0.1))
}
PlotLastGraph <- function(tab) {
  layout(matrix(1:4, nrow = 2))
  plot(tab$diff[tab$periodnum == 5] - tab$swap.close[tab$periodnum == 5]*tab$daystillexpir[tab$periodnum==5], type = "l",
       xlab = "?????", ylab = "???????? 2 ??????? --- 16 ?????", ylim = c(-0.2, 0.2))
  abline(h = 0,  col = "red")
  plot(tab$diff[tab$periodnum == 6] - tab$swap.close[tab$periodnum == 6]*tab$daystillexpir[tab$periodnum==6], type = "l",
       xlab = "?????", ylab = "???????? 17 ????? --- 15 ????", ylim = c(-0.2, 0.2))
  abline(h = 0,  col = "red")
  plot(tab$diff[tab$periodnum == 7] - tab$swap.close[tab$periodnum == 7]*tab$daystillexpir[tab$periodnum==7], type = "l",
       xlab = "?????", ylab = "???????? 16 ???? --- 15 ????????", ylim = c(-0.2, 0.2))
  abline(h = 0,  col = "red")
  plot(tab$diff[tab$periodnum == 8] - tab$swap.close[tab$periodnum == 8]*tab$daystillexpir[tab$periodnum==8], type = "l",
       xlab = "?????", ylab = "???????? ? 16 ???????", ylim = c(-0.2, 0.2))
  abline(h = 0,  col = "red")
}
MegaNetTest <- function(gtab, model, periodnum, size) {
  gtabred <- gtab[gtab$periodnum == periodnum, ]
  modelred <- model[model$periodnum == periodnum, ]
  params <- data.frame(size = size, shift = size, control = 1)
  res <- StrangeStrategyForLib(gtabred, modelred, params, internal = FALSE, simplemode = FALSE)
  return(res)
}
MegaNetTestFull <- function(gtab, model) {
  res <- data.frame(periodnum = numeric(), size = numeric(), numofdeals = numeric(), profit = numeric())
  for (size in seq(0.005, 0.1, 0.005)) {
    res5 <- MegaNetTest(gtab, model, 5, size)
    res <- rbind(res, data.frame(periodnum = 5, size = size, numofdeals = mean(diff(res5$numbersofdeals)), profit = mean(res5$cashes)))
    res6 <- MegaNetTest(gtab, model, 6, size)
    res <- rbind(res, data.frame(periodnum = 6, size = size, numofdeals = mean(diff(res6$numbersofdeals)), profit = mean(res6$cashes)))
    res7 <- MegaNetTest(gtab, model, 7, size)
    res <- rbind(res, data.frame(periodnum = 7, size = size, numofdeals = mean(diff(res7$numbersofdeals)), profit = mean(res7$cashes)))
    res8 <- MegaNetTest(gtab, model, 8, size)
    res <- rbind(res, data.frame(periodnum = 8, size = size, numofdeals = mean(diff(res8$numbersofdeals)), profit = mean(res8$cashes)))
  }
  reshape(res, direction = "wide", timevar = "periodnum", idvar = "size")
}
CreateModelForSimpleNet <- function(gtab) {
  vec <- c()
  for (periodnum in unique(gtab$periodnum)) {
    gtabred <- gtab[gtab$periodnum == periodnum, ]
    print((gtabred$Bid[1] + gtabred$Ask[1])/2)
    vec <- c(vec, rep((gtabred$Bid[1] + gtabred$Ask[1])/2, nrow(gtabred)))
    print(length(vec))
  }
  vec
}
PlotGraphsForSpread <- function(gtab, model, size) {
  res5 <- MegaNetTest(gtab, model, 5, size)
  res6 <- MegaNetTest(gtab, model, 6, size)
  res7 <- MegaNetTest(gtab, model, 7, size)
  res8 <- MegaNetTest(gtab, model, 8, size)
  #  return(list(res5 = res5, res6 = res6, res7 = res7, res8 = res8))
  dates5 <- unique(gtab$Date[gtab$periodnum == 5])
  dates6 <- unique(gtab$Date[gtab$periodnum == 6])
  dates7 <- unique(gtab$Date[gtab$periodnum == 7])
  dates8 <- unique(gtab$Date[gtab$periodnum == 8])

  print(length(dates5))
  print(length(res5$cashes))
  print(length(diff(c(0, res5$numbersofdeals))))

  dres5 <- data.frame(periodnum = rep(5, length(res5$cashes)), date = dates5, numbersofdeals=c(res5$numbersofdeals[1], diff(res5$numbersofdeals)), profit = res5$cashes)
  dres6 <- data.frame(periodnum = rep(6, length(res6$cashes)), date = dates6, numbersofdeals=c(res6$numbersofdeals[1], diff(res6$numbersofdeals)), profit = res6$cashes)
  dres7 <- data.frame(periodnum = rep(7, length(res7$cashes)), date = dates7, numbersofdeals=c(res7$numbersofdeals[1], diff(res7$numbersofdeals)), profit = res7$cashes)
  dres8 <- data.frame(periodnum = rep(8, length(res8$cashes)), date = dates8, numbersofdeals=c(res8$numbersofdeals[1], diff(res8$numbersofdeals)), profit = res8$cashes)

#  return(rbind(dres5, dres6, dres7, dres8))


  print(length(dates5))
  print(length(res5$numbersofdeals))
  print(length(dates6))
  print(length(res6$numbersofdeals))
  print(length(dates7))
  print(length(res7$numbersofdeals))
  print(length(dates8))
  print(length(res8$numbersofdeals))
  print("values")
  tvec <- c(diff(res5$numbersofdeals), diff(res6$numbersofdeals), diff(res7$numbersofdeals), diff(res8$numbersofdeals))


  jpeg(paste0("deals_noncum_size_", size, ".jpeg"), width = 1900, height = 1200)
  layout(matrix(1:4, nrow = 2))
  plot(as.Date(dates5)[1:(length(dates5)-1)], diff(res5$numbersofdeals), ylab = "?????????? ?????? 2 ??????? --- 16 ?????", xlab = "????", type = "l", ylim = c(min(tvec), max(tvec)))
  plot(as.Date(dates6)[1:(length(dates6)-1)], diff(res6$numbersofdeals), ylab = "?????????? ?????? 17 ????? --- 15 ????", xlab = "????", type = "l", ylim = c(min(tvec), max(tvec)))
  plot(as.Date(dates7)[1:(length(dates7)-1)], diff(res7$numbersofdeals), ylab = "?????????? ?????? 16 ???? --- 15 ????????", xlab = "????", type = "l", ylim = c(min(tvec), max(tvec)))
  plot(as.Date(dates8)[1:(length(dates8)-1)], diff(res8$numbersofdeals), ylab = "?????????? ?????? ? 16 ????????", xlab = "????", type = "l", ylim = c(min(tvec), max(tvec)))
  dev.off()
  jpeg(paste0("profits_noncum_size_", size, ".jpeg"), width = 1900, height = 1200)
  layout(matrix(1:4, nrow = 2))
  plot(as.Date(dates5), (res5$cashes), ylab = "??????? 2 ??????? --- 16 ?????", xlab = "????")
  plot(as.Date(dates6), (res6$cashes), ylab = "??????? 17 ????? --- 15 ????", xlab = "????")
  plot(as.Date(dates7), (res7$cashes), ylab = "??????? 16 ???? --- 15 ????????", xlab = "????")
  plot(as.Date(dates8), (res8$cashes), ylab = "??????? ? 16 ????????", xlab = "????")
  dev.off()
}
AshotPlot1 <- function(tab) {
  layout(matrix(1:3, nrow = 3, ncol = 1))
  plot(tab$time, (tab$stavkabid + tab$stavkaask)/2, col = "blue", ylab = "Ставка")
  points(tab$time, tab$daystillexpir*tab$swap.close, col = "red")
  legend("bottomright", legend = c("Ставка", "Своп"), col = c("blue", "red"), cex = 0.4, lty = 1)
  plot(tab$time, tab$usd.close, ylab = "Спот")
  plot(tab$time, tab$si.close, ylab = "Фьючерс")
}
MakeAshotPlot <- function(datestart, dateend, curfut, datename) {
  tab <- FillStavkaTab(datestart, dateend, futsec = curfut, candletype = "1sVol50")
  tab <- tab[seq(1, nrow(tab), 30), ]
  tab$id <- 1:nrow(tab)
  ids <- aggregate(tab$id, list(tab$si.date), max)
  print(ids)
  print(summary(tab))
  jpeg(paste0("ashotplot", datename, ".jpeg"), width = 1900, height = 1200)
  layout(matrix(1:2, nrow = 2))
  plot(tab$id, (tab$stavkabid + tab$stavkaask)/2, ylab = "Ставка", main = datename)
  for (j in 1:nrow(ids)) {
    abline(v = ids[j, 2])
  }
  plot(tab$id, tab$si.close, ylab = "Фьючерс")
  for (j in 1:nrow(ids)) {
    abline(v = ids[j, 2])
  }
  dev.off()
  tab
}
AshotPlot2 <- function(tab) {
  layout(matrix(1:2, nrow = 3, ncol = 1))
  plot(tab$time, (tab$stavkabid + tab$stavkaask)/2 - tab$daystillexpir*tab$swap.close,
       ylim = c(-0.65, 0.65), ylab = "Ставка")
#  points(tab$time, tab$daystillexpir*tab$swap.close, col = "red")
#  legend("bottomright", legend = c("Ставка", "Своп"), col = c("blue", "red"), cex = 0.4, lty = 1)
  plot(tab$time, tab$usd.close, ylab = "Спот")
  plot(tab$time, tab$si.close, ylab = "Фьючерс")
}
AshotPlot3 <- function(tab) {
  layout(matrix(1:3, nrow = 3, ncol = 1))
  plot(tab$time, 100*365*(tab$stavkabid + tab$stavkaask)/(2*tab$usd.close*tab$daystillexpir), col = "blue", ylab = "Ставка")
  points(tab$time, 100*365*tab$swap.close/tab$usd.close, col = "red")
  legend("topright", legend = c("Ставка", "Своп"), col = c("blue", "red"), cex = 0.4, lty = 1)
  plot(tab$time, tab$usd.close, ylab = "Спот")
  plot(tab$time, tab$si.close, ylab = "Фьючерс")
}
AshotPlot4 <- function(tab) {
  layout(matrix(1:3, nrow = 3, ncol = 1))
  plot(tab$time, 100*365*(tab$stavkabid + tab$stavkaask)/(2*tab$usd.close*tab$daystillexpir)
       - 100*365*tab$swap.close/tab$usd.close, ylim = c(-5, 5), ylab = "Ставка")
  #  points(tab$time, tab$daystillexpir*tab$swap.close, col = "red")
  #  legend("bottomright", legend = c("Ставка", "Своп"), col = c("blue", "red"), cex = 0.4, lty = 1)
  plot(tab$time, tab$usd.close, ylab = "Спот")
  plot(tab$time, tab$si.close, ylab = "Фьючерс")
}
AshotPlot5 <- function(tab) {
  layout(matrix(1:2, nrow = 2, ncol = 1))
  plot(tab$dol.time, 100*365*(tab$dol.stavkabid + tab$dol.stavkaask)/(2*tab$dol.usd.close*tab$dol.daystillexpir), col = "blue", ylab = "Ставки")
  points(tab$dol.time, 100*365*(tab$euro.stavkabid + tab$euro.stavkaask)/(2*tab$euro.usd.close*tab$euro.daystillexpir), col = "red")
  legend("topright", col = c("blue", "red"), cex = 0.4, lty = 1, legend = c("dollar", "euro"))
  plot(tab$dol.time, 100*365*(tab$dol.stavkabid + tab$dol.stavkaask)/(2*tab$dol.usd.close*tab$dol.daystillexpir) -
         100*365*(tab$euro.stavkabid + tab$euro.stavkaask)/(2*tab$euro.usd.close*tab$euro.daystillexpir), ylab = "Разность", ylim = c(-5, 5))
}
AshotPlot6 <- function(tab) {
  layout(matrix(1:2, nrow = 2, ncol = 1))
  plot(tab$dol.time, (tab$dol.stavkabid + tab$dol.stavkaask)/2, col = "blue", ylab = "Ставки")
  points(tab$dol.time, (tab$euro.stavkabid + tab$euro.stavkaask)/2, col = "red")
  legend("bottomright", col = c("blue", "red"), cex = 0.4, lty = 1, legend = c("dollar", "euro"))
  plot(tab$dol.time, (tab$dol.stavkabid + tab$dol.stavkaask)/2
       - (tab$euro.stavkabid + tab$euro.stavkaask)/2, ylab = "Разность")

}
