###requires Comparison.R, war_script.R, DollarStavkaGraphs.R
PlotVariousGrapsForSergey <- function(datestart, dateend, variousvolumes = TRUE) {
  t1 <-   PlotVariousGrapsForSergeyPerSec(datestart, dateend, "SBER@TQBR", "SRM6@FORTS", "1s", 100, variousvolumes = variousvolumes)
  t2 <- PlotVariousGrapsForSergeyPerSec(datestart, dateend, "GAZP@TQBR", "GZM6@FORTS", "1s", 100, variousvolumes = variousvolumes)
  t3 <- PlotVariousGrapsForSergeyPerSec(datestart, dateend, "LKOH@TQBR", "LKM6@FORTS", "1s", 10, variousvolumes = variousvolumes)
  t4 <- PlotVariousGrapsForSergeyPerSec(datestart, dateend, "VTBR@TQBR", "VBM6@FORTS", "1s", 100000, variousvolumes = variousvolumes)
list(t1 = t1, t2 = t2, t3 = t3, t4 = t4)
}
PlotVariousGrapsForSergeyPerSec <- function(datestart, dateend, spotsec, futsec, candletype, mod, variousvolumes = TRUE) {
  tab <- FillStavkaTab(datestart, dateend, spotsec = spotsec, futsec = futsec, candletype = "1s", mod = mod)
  print(summary(tab))
  temp <- GetPlotsForMeComparison(tab, NA, "sergey", futsec, datestart, dateend, curspotname = spotsec, variousvolumes = variousvolumes)
  temp
}
WriteDataForSergey <- function(temptab, filename) {
  temptab <- temptab$ctab
  names(temptab) <- c("Time", "quote.bid", "quote.ask", "quote.date", "quote.midprice",
                      "hedge.bid", "hedge.ask", "hedge.date", "hedge.midprice",
                      "marketstavkabid", "marketstavkaask", "marketstavka",
                      "Date", "id", "deal1.Time", "deal1.Security", "deal1.Volume",
                      "deal1.Price", "deal1.Direction", "deal1.Date", "deal1.hedgeid",
                      "deal1.hedgeidmin", "deal2.Security", "deal2.Volume", "deal2.Price",
                      "deal2.Direction", "deal2.Date", "deal2.id", "diff", "deal.stavka", "hedge.time.difference")
  write.table(temptab, filename, sep = ";", col.names = TRUE, row.names = FALSE)
}
GetGroupedDataForStocks <- function(file1, file2, secname1, secname2, multip, metro = TRUE) {
  if (metro) {
    deals1 <- GetDataFromMetroplexPart(file1, secname1)
    deals2 <- GetDataFromMetroplexPart(file2, secname2, weight = 10)
  } else {
    deals <- GetWarDeals(file1)
    deals$Volume <- as.numeric(deals$Volume)
    deals$Price <- as.numeric(deals$Price)
    deals[, Date := as.Date(Time)]
    names(deals)[5] <- "Side"
    deals1 <- deals[Security == secname1, ]
    deals2 <- deals[Security == secname2, ]
    deals2[, Volume := 10*Volume*ifelse(Side == "Buy", 1, -1)]
    deals1[, Volume := Volume*ifelse(Side == "Buy", 1, -1)]
    deals1[, id := 1:nrow(deals1)]
  }
  print(summary(deals1))
  print(summary(deals2))
  print(nrow(deals2))
  deals <- HedgeComparison2(list(deals = deals2, orders = deals1), eps = 5)
  print("hedge done")
  print(summary(deals))
  mtab <- GetHedgeDiff(list(deals = deals2, orders = deals1), deals, TRUE)
  print(mtab$val)
  mtab <- mtab$tab
  print(names(mtab))
  mtab <- (data.frame(mtab$Time, mtab$Security, mtab$Volume, mtab$Price, mtab$Side, mtab$Date,
                mtab$hedgeid, mtab$hedgeidmin, mtab$time, mtab$security, mtab$volume, mtab$price, mtab$side,
                mtab$date, mtab$id, mtab$diff))
  print(dim(mtab))
  deals <- as.data.table(mtab)
  print(dim(mtab))
  print(dim(deals))
  names(deals) <- c("fut.Time", "fut.Security", "fut.Volume", "fut.Price", "fut.Direction",
                   "fut.Date", "fut.hedgeid", "fut.hedgeidmin",
                   "spot.Time", "spot.Security", "spot.Volume",
                   "spot.Price", "spot.Direction", "spot.Date", "spot.id", "diff")
  print(names(deals))
  deals[, stavka := (fut.Price - multip*spot.Price)]
}

GetSergeyData <- function(secname, volume, startdate = as.Date("2015-12-16"), enddate = Sys.Date()) {
  candle <- paste0("Vol", volume)
  tab <- LoadBp(instrument = secname, start.date = startdate, end.date = enddate, candle.type = candle,
                storage.path = "D:\\ForSergey")
  tab <- as.data.frame(na.omit(tab))
  mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"))
  tab <- tab[mask>0, ]
  tab$Close <- (tab$Bid + tab$Ask)/2
  names(tab) <- tolower(names(tab))
  ind <- Purify2(tab, len = 900, strname = "")
  if (length(ind) > 0) {
    return(tab[-ind, ])
  } else {
    return(tab)
  }
}
CompareSpreadsForSergey <- function(secname, volume, ylims) {
  tab1 <- GetSergeyData(secname, 1)
  tab2 <- GetSergeyData(secname, volume)
  tab <- InnerMergeDf(list(v1 = tab1, v2 = tab2), "time")
  mystr <- paste0(paste("pict", secname, volume, sep = "_"), ".jpeg")
  print(mystr)
  layout(1)
  jpeg(paste0("C:\\ForSergey\\Bids_", mystr), width = 1900, height = 1200)
  boxplot(I(tab$v2.bid - tab$v1.bid) ~ tab$v1.date, ylab = paste0("Bid for volume ", volume, "- bid for volume 1."), ylim = c(-ylims, 0))
  dev.off()
  jpeg(paste0("C:\\ForSergey\\Asks_", mystr), width = 1900, height = 1200)
  boxplot(I(tab$v2.ask - tab$v1.ask) ~ tab$v1.date, ylab = paste0("Ask for volume ", volume, "- ask for volume 1."), ylim = c(0, ylims))
  dev.off()
  jpeg(paste0("C:\\ForSergey\\MidPrices_", mystr), width = 1900, height = 1200)
  boxplot(I(tab$v2.close - tab$v1.close) ~ tab$v1.date, ylab = paste0("Middle price for volume ", volume, "- middle price for volume 1."), ylim = c(-ylims/2, ylims/2))
  dev.off()
tab
}

MainScript <- function() {
  res <- CompareSpreadsForSergey("GAZP@TQBR", 1000, 0.3)
  res <- CompareSpreadsForSergey("GAZP@TQBR", 2000, 0.3)
  res <- CompareSpreadsForSergey("GAZP@TQBR", 3000, 0.3)
  res <- CompareSpreadsForSergey("GAZP@TQBR", 4000, 0.3)
  res <- CompareSpreadsForSergey("GAZP@TQBR", 5000, 0.3)
  res <- CompareSpreadsForSergey("LKOH@TQBR", 500, 4)
  res <- CompareSpreadsForSergey("LKOH@TQBR", 1000, 4)
  res <- CompareSpreadsForSergey("LKOH@TQBR", 1500, 4)
  res <- CompareSpreadsForSergey("SBER@TQBR", 1000, 0.2)
  res <- CompareSpreadsForSergey("SBER@TQBR", 2000, 0.2)
  res <- CompareSpreadsForSergey("SBER@TQBR", 3000, 0.2)
  res <- CompareSpreadsForSergey("SBER@TQBR", 4000, 0.2)
  res <- CompareSpreadsForSergey("SBER@TQBR", 5000, 0.2)
  res <- CompareSpreadsForSergey("SBER@TQBR", 6000, 0.2)
  res <- CompareSpreadsForSergey("SBER@TQBR", 7000, 0.2)
  res <- CompareSpreadsForSergey("VTBR@TQBR", 1000, 0.0005)
  res <- CompareSpreadsForSergey("VTBR@TQBR", 2000, 0.0005)
  res <- CompareSpreadsForSergey("VTBR@TQBR", 3000, 0.0005)
  res <- CompareSpreadsForSergey("VTBR@TQBR", 4000, 0.0005)
  res <- CompareSpreadsForSergey("VTBR@TQBR", 5000, 0.0005)
  res <- CompareSpreadsForSergey("VTBR@TQBR", 6000, 0.0005)
}

CalculateBamblebeeModel <- function(tab, alpha) {
  tab <- as.data.table(tab)
  setkey(tab, "mix.date")
  tab[, bid := mix.bid - alpha*0.00002*rts.bid*si.bid]
  tab[, ask := mix.ask - alpha*0.00002*rts.ask*si.ask]
  print("first")
  indik1 <- RollMeanIndik(tab, 2000)
  indik1 <- data.table(Date = tab$mix.date, Val = indik1)
  setkey(indik1, "Date")
  tabmin <- tab[seq(1, nrow(tab), 60), ]
  print("second")
  indik2 <- RollMeanIndik(tabmin, 3330)
  indik2 <- data.table(Date = tabmin$mix.date, Val = indik2)
  setkey(indik2, "Date")
  print("third")
  for (adate in unique(tab$mix.date)) {
    indik2red <- indik2[Date == adate, ]
    beta <- indik2red$Val[1]
    indik1[Date == adate, Val := 0.8*Val + 0.2*beta]
  }
  print(head(tab))
  print(head(indik1$Val))
  tab$diff <- tab$mix.close - alpha*0.00002*tab$rts.bid*tab$si.bid/2 -
    alpha*0.00002*tab$rts.ask*tab$si.ask/2 - indik1$Val
tab
}
