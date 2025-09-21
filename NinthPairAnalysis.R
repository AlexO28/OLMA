FillDiffInZeroPoints <- function(zeros) {
  setkey(zeros, Date)
  zeros[, mydiff := as.numeric(as.Date("2016-12-16") - as.numeric(zeros$Date))]
  zeros[, newdiff := c(mydiff[2:(length(mydiff))], mydiff[length(mydiff)]-1)]
zeros
}
GetZeroPointsByDates <- function(dates, spread, shift) {
  for (adate in dates) {
    if (!(as.POSIXlt(as.Date(adate), origin = origin)$wday %in% c(0, 6))) {
      zerotab <- GetZeroPointsForNinthPair(as.Date(adate), spread, shift)
      if (is.null(nrow(zerotab))) {next}
      if (nrow(zerotab) == 0) {next}
      zeropoints <- zerotab$ZeroPoint
      cvals <- BootStrapMeanEstimate(zeropoints)
      write.table(data.frame(as.Date(adate), cvals$estimate,
                             cvals$conf.int[1], cvals$conf.int[2]), "NinthPair2\\zeropoints.txt",
                  row.names = FALSE, col.names = FALSE, sep = ";", append = TRUE)
    }
  }
}
GetZeroPointsForNinthPair <- function(adate, spread, shift) {
  SetGlobalOptions()
  ninthpair <- GetTabsForNinthPair(adate)
  posstart <- ninthpair$posstart
  ninthpair <- ninthpair$res

  rm(tabspot)
  tabspot <- GetQuotesData("USD000UTSTOM@CETS", adate, candletype)
  tabspot[, Bid := 1000*Bid]
  tabspot[, Ask := 1000*Ask]

  futdeals <- rbind(ninthpair$firstmatched, ninthpair$firstnotmatched)
#  futdeals[, Pos := cumsum(ifelse(Direction == "Buy", 1, -1))]
  if (is.na(posstart)) {return(NA)}
  setkey(futdeals, "Time")

  htab <- JoinFutAndDeals(tabspot, futdeals)
  htab[, Pos := Pos + posstart]
  htab[, Val := Price - ifelse(Direction == "Sell", Ask, Bid)]
  htab[, ZeroPoint := Val +
             ifelse(Direction == "Sell", -spread/2, spread/2) +
             shift*Pos]
}
CalculateSpreadsFromData <- function(resspreads) {
  res <- data.frame(Date = as.Date(NA),
                    sell1 = as.numeric(NA),
                    sell2 = as.numeric(NA),
                    buy1 = as.numeric(NA),
                    buy2 = as.numeric(NA),
                    spread1 = as.numeric(NA),
                    spread2 = as.numeric(NA),
                    vol1 = as.numeric(NA),
                    vol2 = as.numeric(NA))
  for (j in 1:nrow(resspreads)) {
    kefsell1 <- resspreads$V2[j]
    kefsell2 <- resspreads$V3[j]
    kefbuy1 <- resspreads$V4[j]
    kefbuy2 <- resspreads$V5[j]
    vol1 <- (resspreads$V10[j] + resspreads$V12[j])/2
    vol2 <- (resspreads$V11[j] + resspreads$V13[j])/2
    sellpoint1 <- kefsell1 + kefsell2*vol1
    buypoint1 <- kefbuy1 + kefbuy2*vol1
    sellpoint2 <- kefsell1 + kefsell2*vol2
    buypoint2 <- kefbuy1 + kefbuy2*vol2
    res <- rbind(res, data.frame(Date = resspreads$V1[j],
                                 sell1 = sellpoint1,
                                 sell2 = sellpoint2,
                                 buy1 = buypoint1,
                                 buy2 = buypoint2,
                                 spread1 = (sellpoint1 - buypoint1),
                                 spread2 = (sellpoint2 - buypoint2),
                                 vol1 = vol1,
                                 vol2 = vol2))
  }
na.omit(res)
}
InvestigateAllData <- function(dates, afile) {
  SetGlobalOptions()
  for (adate in dates) {
    if (!(as.POSIXlt(as.Date(adate), origin = origin)$wday %in% c(0, 6))) {
      res <- GetAllDataForOneDay(as.Date(adate))
      write.table(data.frame(Date = as.Date(adate),
                 # fut.estimate = res$fut$estimate,
                #  fut.conf.int1 = res$fut$conf.int[1],
                #  fut.conf.int2 = res$fut$conf.int[2],
                  spot.estimate.plus = res$spot$plus.estim,
                  spot.conf.int.plus1 = res$spot$plus.conf.int[1],
                  spot.conf.int.plus2 = res$spot$plus.conf.int[2],
                  spot.estimate.minus = res$spot$minus.estim,
                  spot.conf.int.minus1 = res$spot$minus.conf.int[1],
                  spot.conf.int.minus2 = res$spot$minus.conf.int[2],
                  spot.estimate = res$spot$estim,
                  spot.conf.int1 = res$spot$conf.int[1],
                  spot.conf.int2 = res$spot$conf.int[2]),
                  afile, row.names = FALSE, col.names = FALSE, sep = ";", append = TRUE)
      gc()
    }
  }
}
InvestigateAllRegrs <- function(dates) {
  for (adate in dates) {
    if (!(as.POSIXlt(as.Date(adate), origin = origin)$wday %in% c(0, 6))) {
      res <- GetRegrDataForOneDay(as.Date(adate))
    }
  }
}
MakePlotNinthPair <- function(adate) {
  ninthpair <- GetTabsForNinthPair(adate)
  futdeals <- rbind(ninthpair$firstmatched, ninthpair$firstnotmatched)
  setkey(futdeals, "Time")
  spotdeals <- rbind(ninthpair$secondmatched, ninthpair$secondnotmatched)
  setkey(spotdeals, "Time")
  comdeals <- ninthpair$matched
  tabfut <- as.data.frame(GetQuotesData("SIZ6@FORTS", adate, "1s"))
  tabspot <- as.data.frame(GetQuotesData("USD000UTSTOM@CETS", adate, "1s"))
  tab <- InnerMergeDf(list(spot = tabspot, fut = tabfut), key = "Time")
  tab[, stavka := (fut.Bid + fut.Ask)/2 - 1000*(spot.Bid + spot.Ask)]
  print("completed")
  layout(matrix(1:3))
  plot(tab$Time, tab$stavka)
  for (j in 1:nrow(comdeals)) {
    if (comdeals$Direction[j] == "Sell") {
      points(comdeals$Time[j], comdeals$Price[j] - 1000*comdeals$i.Price[j], col = "red", bg = "red", pch = 25)
    } else {
      points(comdeals$Time[j], comdeals$Price[j] - 1000*comdeals$i.Price[j], col = "green", bg = "green", pch = 24)
    }
  }
  plot(tabfut$Time, (tabfut$Bid + tabfut$Ask)/2)
  for (j in 1:nrow(futdeals)) {
    if (futdeals$Direction[j] == "Sell") {
      points(futdeals$Time[j], futdeals$Price[j], col = "red", bg = "red", pch = 25)
    } else {
      points(futdeals$Time[j], futdeals$Price[j], col = "green", bg = "green", pch = 24)
    }
  }
  plot(tabdol$Time, (tabdol$Bid + tabdol$Ask)/2)
  for (j in 1:nrow(spotdeals)) {
    if (spotdeals$Direction[j] == "Sell") {
      points(spotdeals$Time[j], spotdeals$Price[j], col = "red", bg = "red", pch = 25)
    } else {
      points(spotdeals$Time[j], spotdeals$Price[j], col = "green", bg = "green", pch = 24)
    }
  }
}
GetRegrDataForOneDay <- function(adate) {
  ninthpair <- GetTabsForNinthPair(adate)
  posstart <- ninthpair$posstart
  ninthpair <- ninthpair$res

  futdeals <- rbind(ninthpair$firstmatched, ninthpair$firstnotmatched)
  setkey(futdeals, "Time")
  rm(tabspot)
  tabspot <- GetQuotesData("USD000UTSTOM@CETS", adate, candletype)
  tabspot[, Bid := 1000*Bid]
  tabspot[, Ask := 1000*Ask]

  htab <- JoinFutAndDeals(tabspot, futdeals)
  if (!is.na(posstart)) {
    htab[, Pos := Pos + posstart]
  } else {
    return(NA)
  }
  stab <- StudyFutDeals(htab)

  stabsell <- stab$sell
  stabbuy <- stab$buy
 # stabsell[, Pos := Pos + posstart]
 #  stabbuy[, Pos := Pos + posstart]

  reg1 <- rlm(Val ~ Pos, data = stabsell)
  reg2 <- rlm(Val ~ Pos, data = stabbuy)
  rdat <- data.frame(Date = as.Date(adate),
                     sellkef1 = as.numeric(coef(reg1)[1]),
                     sellkef2 = as.numeric(coef(reg1)[2]),
                     buykef1 = as.numeric(coef(reg2)[1]),
                     buykef2 = as.numeric(coef(reg2)[2]),
                     sellrse = reg1$s,
                     buyrse = reg2$s,
                     selldegs = nrow(stabsell) - 2,
                     buydegs = nrow(stabbuy) - 2,
                     possellmin = min(stabsell$Pos),
                     possellmax = max(stabsell$Pos),
                     posbuymin = min(stabbuy$Pos),
                     posbuymax = max(stabbuy$Pos))
  write.table(rdat, "NinthPair2\\spreads.txt", sep = ";", col.names = FALSE, row.names = FALSE, append = TRUE)
  print(rdat)
  jpeg(paste0("NinthPair2\\", gsub("-", "", adate), ".jpeg"), width = 1900, height = 1200)
  layout(matrix(1:2))
  plot(stabsell$Pos, stabsell$Val)
  abline(reg1, col = "red")
  plot(stabbuy$Pos, stabbuy$Val)
  abline(reg2, col = "red")
  dev.off()
}
GetAllDataForOneDay <- function(adate) {
  ninthpair <- GetTabsForNinthPair(adate)
  posstart <- ninthpair$posstart
  ninthpair <- ninthpair$res

  futdeals <- rbind(ninthpair$firstmatched, ninthpair$firstnotmatched)
  setkey(futdeals, "Time")
  comdeals <- ninthpair$matched

  #candletype <- "1s"
  candletype <- "Analysis"

  #tabfut <- GetQuotesData("SIZ6@FORTS", adate, candletype)
  rm(tabspot)
  tabspot <- GetQuotesData("USD000UTSTOM@CETS", adate, candletype)
  #resfut <- GetFullFutInfo(tabfut, futdeals)
  resspot <- GetFullSpotInfo(tabspot, comdeals)
list(spot = resspot)
}
JoinFutAndDeals <- function(tabfut, futdeals) {
  tabfut[, TimeNum := as.numeric(Time)]
  futdeals[, TimeNum := as.numeric(Time)]
  setkey(tabfut, "TimeNum")
  setkey(futdeals, "TimeNum")
  futdeals[, Pos := cumsum(ifelse(Direction == "Buy", 1, -1))]
  htab <- tabfut[futdeals, roll = TRUE]
#  print(head(htab))
htab[!duplicated(i.id), ]
}
StudyFutDeals <- function(htab, reversemode = FALSE) {
  htabsell <- htab[Direction == "Sell", ]
  htabbuy <- htab[Direction == "Buy", ]
  if (!reversemode) {
    htabsell[, Val := Price - Ask]
    htabbuy[, Val := Price - Bid]
  } else {
    htabsell[, Val := Price - Bid]
    htabbuy[, Val := Price - Ask]
  }
list(sell = htabsell, buy = htabbuy)
}
GetQuotesData <- function(instrname, adate, candletype) {
  tab <- na.omit(LoadBp(instrname, adate, adate, candletype, storage.path = "D:\\", fast = FALSE))
  tab[, id := 1:nrow(tab)]
}
GetFullFutInfo <- function(tabfut, futdeals) {
  htab <- JoinFutAndDeals(tabfut, futdeals)
  stab <- StudyFutDeals(htab)
  stabsell <- stab$sell
  stabbuy <- stab$buy
  vals <- c(stabsell$Val, -stabbuy$Val)
#  print(summary(vals))
  cvals <- BootStrapMeanEstimate(vals)
list(conf.int = round(cvals$conf.int),
     estimate = round(cvals$estimate))
}
GetFullSpotInfo <- function(tabdol, comdeals) {
  comdeals2 <- data.table(Time = comdeals$Time, Direction = comdeals$i.Direction,
                          Price = comdeals$i.Price)
  comdeals2[, id := 1:nrow(comdeals2)]
  htab <- JoinFutAndDeals(tabdol, comdeals2)
  stab <- StudyFutDeals(htab)

  stabsell <- stab$sell
  stabbuy <- stab$buy
  stabsell[, Val := round(Val/0.0025)]
  stabbuy[, Val := -round(Val/0.0025)]
  vals <- c(stabsell$Val, stabbuy$Val)
  valsplus <- vals[vals > 0]
  valsminus <- vals[vals < 0]
#list(medplus = median(valsplus), medminus = median(valsminus),
#     plus = valsplus, minus = valsminus)
  cvalsplus <- BootStrapMeanEstimate(valsplus)
  cvalsminus <- BootStrapMeanEstimate(valsminus)
  cvals <- BootStrapMeanEstimate(vals)
list(plus.conf.int = round(cvalsplus$conf.int),
     plus.estim = round(cvalsplus$estimate),
     minus.conf.int = round(cvalsminus$conf.int),
     minus.estim = round(cvalsminus$estimate),
     conf.int = round(cvals$conf.int),
     estim = round(cvals$estimate))
}
