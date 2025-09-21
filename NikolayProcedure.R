TripleBoxPlotByDates <- function(tabb, datestart, dateend) {
  tab <- as.data.table(tabb)
  tab[, val := euro.close - pair.close*dol.close]
  tabred <- tab[dol.date >= datestart & dol.date <= dateend, ]
  print(summary(tabred))
  boxplot(val ~ dol.date, data = tabred, ylim = c(-100, max(tabred$val)))
  print(quantile(tabred$val, 0.9))
  abline(h = quantile(tabred$val, 0.9), col = "red")
  print(quantile(tabred$val, 0.1))
  abline(h = quantile(tabred$val, 0.1), col = "red")
  print(quantile(tabred$val, 0.95))
  abline(h = quantile(tabred$val, 0.95), col = "blue")
  print(quantile(tabred$val, 0.05))
  abline(h = quantile(tabred$val, 0.05), col = "blue")
}
PlotNikolaiDeals <- function(tab, grouptab) {
  tab <- as.data.table(tab)
  tab[, id := 1:nrow(tab)]
  setkey(tab, "Time")
  setkey(grouptab, "time")
  ctab <- tab[grouptab, roll = TRUE, mult = "first"]
  jpeg("tripleplottot.jpeg", width = 1900, height = 1200)
  plot(tab$id, (tab$euro.close - tab$pair.close*tab$dol.close)/1000, main = "Euro minus pair * dol", ylim = c(-100/1000, 100/1000))
  points(ctab$id[ctab$Volume > 0 ], ctab$Pair[ctab$Volume > 0], col = "green", bg = "green", pch = 24)
  points(ctab$id[ctab$Volume < 0], ctab$Pair[ctab$Volume < 0], col = "red", bg = "red", pch = 25)
  dev.off()
  for (adate in unique(tab$euro.date)) {
    adatech <- as.character(as.Date(adate))
    print(adatech)
    jpeg(paste0("tripleplot_", adatech, ".jpeg"), width = 1900, height = 1200)
    tabred <- tab[euro.date == adate, ]
    ctabred <- ctab[euro.date == adate, ]
    plot(tabred$id, (tabred$euro.close - tabred$pair.close*tabred$dol.close)/1000, main = paste0("Euro minus pair * dol ", adatech), ylim = c(-100/1000, 100/1000))
    if (nrow(ctabred) > 0) {
      points(ctabred$id[ctabred$Volume > 0 ], ctabred$Pair[ctabred$Volume > 0], col = "green", bg = "green", pch = 24)
      points(ctabred$id[ctabred$Volume < 0], ctabred$Pair[ctabred$Volume < 0], col = "red", bg = "red", pch = 25)
    }
    dev.off()
  }
ctab
}

GetTripleData <- function(datestart, dateend, instr1, instr2, instr3, candletype = "1sVol50", topurify = TRUE, type = "fut", expinfo = NA) {
  if (type == "fut") {
    expinfo$fullname <- paste0(instr1, expinfo$shortname, "@FORTS")
    tab1 <- as.data.frame(na.omit(LoadBpVarInstrs(candletype = candletype, storage = "D:\\", expinfo = expinfo)))
    expinfo$fullname <- paste0(instr2, expinfo$shortname, "@FORTS")
    tab2 <- as.data.frame(na.omit(LoadBpVarInstrs(candletype = candletype, storage = "D:\\", expinfo = expinfo)))
    expinfo$fullname <- paste0(instr3, expinfo$shortname, "@FORTS")
    tab3 <- as.data.frame(na.omit(LoadBpVarInstrs(candletype = candletype, storage = "D:\\", expinfo = expinfo)))

    print(summary(tab1))
    print(summary(tab2))
    print(summary(tab3))
    #  return(list(tab1 = tab1, tab2 = tab2, tab3 = tab3))
  } else {
    tab1 <- as.data.frame(LoadBp(instr1, datestart, dateend, candle.type = candletype, storage.path = "D:\\"))
    tab2 <- as.data.frame(LoadBp(instr2, datestart, dateend, candle.type = candletype, storage.path = "D:\\"))
    tab3 <- as.data.frame(LoadBp(instr3, datestart, dateend, candle.type = candletype, storage.path = "D:\\"))
  }
  tab <- as.data.frame(InnerMergeDf(list(pair = tab1,
                           euro = tab2,
                           dol = tab3), "Time"))
  names(tab) <- tolower(names(tab))
  names(tab)[1] <- "Time"
  mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"), tz = "GMT")
  tab <- tab[mask>0, ]
  print("mask chosen")
  print(head(tab))
  if (!topurify) {
    return(tab)
  }
  ind1 <- Purify2(tab, 15*60, strname = "pair.")
  if (length(ind1) > 0) {tab <- tab[-ind1, ]}
  print("purified 1")
  ind2 <- Purify2(tab, 15*60, strname = "euro.")
  if (length(ind2) > 0) {tab <- tab[-ind2, ]}
  print("purified 2")
  ind3 <- Purify2(tab, 15*60, strname = "dol.")
  if (length(ind3) > 0) {tab <- tab[-ind3, ]}
  print("purified 3")

  tab$diff <- tab$euro.close - tab$pair.close*tab$dol.close
  tab2 <- KillQuantiles(tab, 0.05, "diff", 60)
  htab <- aggregate(tab2$pair.bid, list(tab2$pair.date), length)
  print(htab)
  print(summary(htab))
  days <- htab[htab[, 2] >= 20000, 1]
  tab3 <- tab2[tab2$pair.date %in% days, ]
tab3
}
