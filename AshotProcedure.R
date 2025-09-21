AshotBrPlot <- function(tab, tabbr, control = 1) {
#  corfun <- function(datum) {
#    cor(datum$br.close, datum$stavka.stavkaav)
#  }
  ctab <- InnerMergeDf(list(stavka = tab, br = tabbr), "Time")

  for (adate in unique(ctab$br.date)) {
    ctabred <- ctab[br.date == adate, ]
    adatech <- as.character(as.Date(adate))
    print(adatech)
    if (control == 1) {
      jpeg(paste0("ashotbrent_", adatech, ".jpeg"), width = 1900, height = 1200)
      plot(ctabred$br.close, ctabred$stavka.stavkaav)
      abline(lm(stavka.stavkaav ~ br.close, data = ctabred))
      dev.off()
    } else {
      vec <- runCor(ctabred$stavka.stavkaav, ctabred$br.close, n = 3600)
#      stop()
      jpeg(paste0("ashotbrentcor_", adatech, ".jpeg"), width = 1900, height = 1200)
      layout(matrix(1:2, nrow = 2))
      plot(na.omit(vec), main = adatech, ylab = "correlation")
      plot(ctabred$stavka.stavkaav[3600:nrow(ctabred)], ylab = "stavka")
      dev.off()
    }
  }
}
AshotInit <- function() {
tabdol <- GetCleanData("spot", "USD000UTSTOM@CETS",
                       datestart = as.Date("2016-02-02"), dateend = as.Date("2016-02-28"),
                       storage.path = "D:\\", candletype = "1sVol50", timelen = 60)
tabeuro <- GetCleanData("spot", "EUR_RUB__TOM@CETS",
                        datestart = as.Date("2016-02-02"), dateend = as.Date("2016-02-28"),
                        storage.path = "D:\\", candletype = "1sVol50", timelen = 60)
tabsi <- GetCleanData("fut", "SI",
                      datestart = as.Date("2016-02-02"), dateend = as.Date("2016-02-28"),
                      storage.path = "D:\\", candletype = "1sVol50",
                      expinfo = data.frame(start = as.Date("2016-02-02"),
                                           end = as.Date("2016-02-28"),
                                           shortname = "H6",
                                           fullname = NA), timelen = 60)
tabeu <- GetCleanData("fut", "EU",
                      datestart = as.Date("2016-02-02"), dateend = as.Date("2016-02-28"),
                      storage.path = "D:\\", candletype = "1sVol50",
                      expinfo = data.frame(start = as.Date("2016-02-02"),
                                           end = as.Date("2016-02-28"),
                                           shortname = "H6",
                                           fullname = NA), timelen = 60)
tabed <- GetCleanData("fut", "ED",
                      datestart = as.Date("2016-02-02"), dateend = as.Date("2016-02-28"),
                      storage.path = "D:\\", candletype = "1sVol50",
                      expinfo = data.frame(start = as.Date("2016-02-02"),
                                           end = as.Date("2016-02-28"),
                                           shortname = "H6",
                                           fullname = NA), timelen = 60)
tab <- InnerMergeDf(list(usd = tabdol, si = tabsi, euro = tabeuro, eu = tabeu, ed = tabed), "Time")
}
CombinedPlotsForAshot <- function(tab) {
  dates <- unique(tab$usd.date)
  tab[, euro.stavka := eu.close/1000 - euro.close]
  tab[, usd.stavka := si.close/1000 - usd.close]
 # tab[, rel := eu.close/1000 - ed.close*si.close/1000]
  tab[, rel := eu.close/si.close - ed.close]
  tab[, diff := as.numeric(as.Date("2016-03-15")) - as.numeric(usd.date)]
  tab$euro.stavka <- 100*365*tab$euro.stavka/(tab$diff*tab$euro.close)
  tab$usd.stavka <- 100*365*tab$usd.stavka/(tab$diff*tab$usd.close)
  print(summary(tab))
  for (adate in dates) {
    adate <- as.Date(adate)
    print(adate)
    tabred <- tab[usd.date == adate, ]
    jpeg(paste0("pictforashot_", adate, ".jpeg"), width = 1900, height = 1200)
    layout(matrix(1:2, nrow = 2))
    myrange <- c(0.9*min(tabred$euro.stavka, tabred$usd.stavka),
                 1.1*min(1000,max(tabred$euro.stavka, tabred$usd.stavka)))
    print(myrange)
    plot(tabred$Time, tabred$euro.stavka - tabred$usd.stavka, ylab = "Разность аннуализированных ставок",
         col = "green")
#    points(tabred$Time, tabred$usd.stavka, col = "red")
    legend("topright", legend = c("euro", "dol"), col = c("green", "red"), lty = 1)
    plot(tabred$Time, tabred$rel, ylab = "График отклонений")
    dev.off()
  }
}

OldCode <- function() {
tabSI <- as.data.frame(LoadBp("SIH6@FORTS", start.date = as.Date("2015-12-14"), end.date = Sys.Date(), candle.type = "1s", storage.path = "D:"))
tabUSD <- as.data.frame(LoadBp("USD000UTSTOM@CETS", start.date = as.Date("2015-12-14"), end.date = Sys.Date(), candle.type = "1s", storage.path = "D:"))
tabUSD$Bid <- sub(",", ".", tabUSD$Bid)
tabUSD$Bid <- as.numeric(tabUSD$Bid)
tab <- InnerMergeDf(list(si = tabSI, usd = tabUSD), "Time")
tab <- na.omit(tab)
names(tab) <- tolower(names(tab))
tab$Time <- tab$si.time
tab$date <- tab$si.date

mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"))
tab <- tab[mask>0, ]

ind1 <- Purify2(tab, len = 5*60, strname = "si.")
ind2 <- Purify2(tab, len = 5*60, strname = "usd.")
if (length(ind1)>0 | length(ind2)>0) {tab2 <- tab[-union(ind1, ind2), ]} else {tab2 <- tab}

tab2$usd.close <- (tab2$usd.bid + tab2$usd.ask)/2
tab2$si.bid <- tab2$si.bid/1000
tab2$si.ask <- tab2$si.ask/1000
tab2$si.close <- (tab2$si.bid + tab2$si.ask)/2
tab2$diff <- tab2$si.close - tab2$usd.close

tab3 <- KillQuantiles(tab2, 0.05, "diff", 15)

htab <- aggregate(tab3$si.bid, list(tab3$date), length)
print(htab)

tab3$stavkabid <- tab3$si.bid - tab3$usd.ask
tab3$stavkaask <- tab3$si.ask - tab3$usd.bid

quantfun <- function(x) {as.numeric(quantile(x, 0.9)) - as.numeric(quantile(x, 0.1))}

stavkabiddevs <- aggregate(tab3$stavkabid, list(tab3$date), quantfun)
stavkaaskdevs <- aggregate(tab3$stavkaask, list(tab3$date), quantfun)
stavkabidsd <- aggregate(tab3$stavkabid, list(tab3$date), sd)
stavkaasksd <- aggregate(tab3$stavkaask, list(tab3$date), sd)
stavkameanclose <- aggregate((tab3$stavkabid + tab3$stavkaask)/2, list(tab3$date), mean)

ashotstavkadata <- data.frame(time = tab3$Time, date = tab3$date,
                              si.bid = tab3$si.bid, si.ask = tab3$si.ask,
                              usd.bid = tab3$usd.bid, usd.ask = tab3$usd.ask,
                              stavka.bid = tab3$stavkabid, stavka.ask = tab3$stavkaask)
ashotshortstavkadata <- data.frame(Date = stavkabiddevs[, 1],
                                   stavka.bid.deviations = stavkabiddevs[, 2], stavka.ask.deviations = stavkaaskdevs[, 2],
                                   stavka.bid.sd = stavkabidsd[, 2], stavka.ask.sd = stavkaasksd[, 2],
                                   stavka.close.mean = stavkameanclose[, 2])
write.table(ashotstavkadata, "DollarFullStavkaDataForAshot.csv", row.names = FALSE, sep = ";")
write.table(ashotshortstavkadata, "DollarShortStavkaDataForAshot.csv", row.names = FALSE, sep = ";")
plot(ashotstavkadata$time, ashotstavkadata$stavka.bid, type = "l", col = "green",
     ylim = c(min(ashotstavkadata$stavka.bid), max(ashotstavkadata$stavka.ask)))
lines(ashotstavkadata$time, ashotstavkadata$stavka.ask, col = "red")
}

