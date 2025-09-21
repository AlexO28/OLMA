ChangeHedgeData <- function(tab, hedgestrat, poses) {
  posm <- poses[1]
  posr <- GetRiPos(posm, hedgestrat, 1.587214*0.00002*tab$PriceRI[1]*tab$PriceSI[1], tab$PriceRI[1], tab$PriceSI[1])
  poss <- GetSiPos(posr, tab$PriceRI[1], hedgestrat, tab$PriceSI[1])
  print(c(posm, posr, poss))
  restab <- tab
  for (j in 1:nrow(tab)) {
    posm <- posm + tab$Volume[j]
    if (!is.na(tab$PriceRI[j]) & !is.na(tab$PriceSI[j])) {
      posrnew <- GetRiPos(posm, hedgestrat, 1.587214*0.00002*tab$PriceRI[j]*tab$PriceSI[j], tab$PriceRI[j], tab$PriceSI[j])
      possnew <- GetSiPos(posr, tab$PriceRI[j], hedgestrat, tab$PriceSI[j])
      restab$VolumeRI[j] <- posrnew - posr
      restab$VolumeSI[j] <- possnew - poss
      posr <- posrnew
      poss <- possnew
    }
  }
restab
}
GetRiPos <- function(pos, hedgestrat, model = NA, rtsprice = NA, usdprice = NA) {
  if (hedgestrat == 1) {
    return(round(-pos*0.1587214))
  } else if (hedgestrat == 2) {
    return(round(-pos*model/(10*rtsprice*1.328)))
  } else {
    return(round(-pos*model/(10*rtsprice*0.00002*usdprice)))
  }
}
GetSiPos <- function(pos, rtsprice, hedgestrat, usdprice) {
 if (hedgestrat > 0) {
   return(round(1.328*pos*(rtsprice/usdprice)))
 } else {
   return(round(0.00002*pos*rtsprice))
 }
}
GroupRISIData <- function(tab) {
#NB. Volume here is the second column, and Price is the third column
  tabRI <- tab[substr(tab$Security, 1, 1) == "R", ]
  tabSI <- tab[substr(tab$Security, 1, 1) == "S", ]
  tabMI <- tab[substr(tab$Security, 1, 1) == "M", ]
  volumesRI <- aggregate(tabRI$Volume, list(tabRI$mmvbid), sum)[, 2]
  pricesRI <- aggregate(tabRI$Volume*tabRI$Price, list(tabRI$mmvbid), sum)[, 2]
  timesRI <- aggregate(tabRI$time, list(tabRI$mmvbid), max)[, 2]
  pricesRI <- pricesRI/volumesRI
  volumesSI <- aggregate(tabSI$Volume, list(tabSI$mmvbid), sum)[, 2]
  pricesSI <- aggregate(tabSI$Volume*tabSI$Price, list(tabSI$mmvbid), sum)[, 2]
  timesSI <- aggregate(tabSI$time, list(tabSI$mmvbid), max)[, 2]
  pricesSI <- pricesSI/volumesSI
  tabRI <- data.frame(PriceRI = pricesRI, VolumeRI = volumesRI, mmvbid = unique(tabRI$mmvbid),
                      timeRI = timesRI)
  tabSI <- data.frame(PriceSI = pricesSI, VolumeSI = volumesSI, mmvbid = unique(tabSI$mmvbid),
                      timeSI = timesSI)
  tabRS <- merge(tabRI, tabSI, by = "mmvbid", all = TRUE)
  tabRS$VolumeRI[is.na(tabRS$PriceRI)] <- 0
  tabRS$VolumeSI[is.na(tabRS$PriceSI)] <- 0
merge(tabMI, tabRS, by = "mmvbid")
}
GroupMMVBData <- function(tab, raw = TRUE) {
  if (raw) {
    names(tab) <- tolower(names(tab))
    tab$price[tab$security == "MMZ5@FORTS"] <- 100*tab$price[tab$security == "MMZ5@FORTS"]
    tab$direction <- ifelse(tab$direction == "Buy", 1, -1)
    tab$tradetime <- as.POSIXct(tab$tradetime, format = "%d-%m-%Y %H:%M:%OS")
    tab$time <- NULL
    tab$time <- tab$tradetime
  }
  notmmvb <- TRUE
  restab <- data.frame(Security = numeric(), Volume = numeric(), Price = numeric(), mmvbid = numeric(), time = numeric())
  sumvol <- 0
  indcur <- 0
  mmvbid <- 0
  for (j in 1:nrow(tab)) {
    if (substr(tab$security[j], 1, 1) == "M") {
      sumvol <- sumvol + tab$volume[j]*tab$direction[j]
      if (notmmvb) {
        indcur <- indcur + 1
        mmvbid <- mmvbid + 1
        restab <- rbind(restab, data.frame(Security = tab$security[j], Volume = sumvol,
                                           Price = tab$price[j], mmvbid = mmvbid,
                                           time = tab$time[j]))
      } else {
        restab$Volume[indcur] <- sumvol
        restab$time[indcur] <- tab$time[j]
      }
      notmmvb <- FALSE
    } else {
      sumvol <- 0
      indcur <- indcur + 1
      restab <- rbind(restab, data.frame(Security = tab$security[j],
                                         Volume = tab$direction[j]*tab$volume[j],
                                         Price = tab$price[j],
                                         mmvbid = mmvbid,
                                         time = tab$time[j]))
      notmmvb <- TRUE
    }
  }
restab
}
PrepareTradesForPLCalc <- function(tab, poses) {
  tab <- rbind(data.frame(mmvbid = 0, Security = "MMZ5@FORTS", Volume = poses[1],
                               Price = tab$Price[1], PriceRI = tab$PriceRI[1],
                               VolumeRI = poses[2], PriceSI = tab$PriceSI[1],
                               VolumeSI = poses[3]), tab)
  posRI <- sum(tab$VolumeRI)
  posMM <- sum(tab$Volume)
  posSI <- sum(tab$VolumeSI)
  tab <- rbind(tab, data.frame(mmvbid = 100000, Security = "MMZ5@FORTS", Volume = -posMM,
                               Price = tab$Price[nrow(tab)], PriceRI = tab$PriceRI[nrow(tab)],
                               VolumeRI = -posRI, PriceSI = tab$PriceSI[nrow(tab)],
                               VolumeSI = -posSI))
tab
}
CalcProf <- function(tab, val) {
  tab$PriceRI[!is.finite(tab$PriceRI)] <- NA
  tab$PriceSI[!is.finite(tab$PriceSI)] <- NA
  prof1 <- sum(-tab$Volume*tab$Price, na.rm = TRUE)/10
  prof2 <- sum(-tab$VolumeRI*tab$PriceRI, na.rm = TRUE)*val
  prof3 <- sum(-tab$VolumeSI*tab$PriceSI, na.rm = TRUE)
  print(c(prof1, prof2, prof3))
prof1 + prof2 + prof3
}
