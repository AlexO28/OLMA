MainGrouper <- function(tab, datestart, dateend = Sys.Date()) {
  grouptab <- GroupMMVBData(as.data.frame(tab), datestart = datestart, dateend = dateend)
  grouptab2 <- as.data.table(GroupRISIData(as.data.frame(grouptab)))
  grouptab2[, PriceSI := ifelse(VolumeSI == 0, 0, PriceSI)]
  grouptab2[, PriceRI := ifelse(VolumeRI == 0, 0, PriceRI)]
  grouptab2[, Pair := ((Price/1000) + (VolumeRI/Volume)*PriceRI*(PriceSI/1000))]
  grouptab2[, Pair2 := VolumeRI*PriceRI - VolumeSI]
grouptab2
}
GroupMMVBData <- function(tab, raw = TRUE, datestart = "2015-01-01", dateend = "2115-01-01") {
  if (raw) {
    names(tab) <- tolower(names(tab))
    print(head(tab))
    tab$direction <- ifelse(tab$side == "Buy", 1, -1)
    if (is.null(tab$time)) {
      tab$time <- tab$date
    }
    tab <- tab[order(tab$time), ]
    tab$date <- as.Date(tab$time)
    tab <- tab[tab$date >= as.Date(datestart) & tab$date <= as.Date(dateend), ]
  }

  notmmvb <- TRUE
  restab <- data.frame(Security = numeric(), Volume = numeric(), Price = numeric(), mmvbid = numeric(), time = numeric())
  sumvol <- 0
  indcur <- 0
  mmvbid <- 0
  for (j in 1:nrow(tab)) {
    if (substr(tab$security[j], 1, 2) == "EU") {
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
GroupRISIData <- function(tab) {
  #NB. Volume here is the second column, and Price is the third column
  tabRI <- tab[substr(tab$Security, 1, 2) == "ED", ]
  tabSI <- tab[substr(tab$Security, 1, 1) == "S", ]
  tabMI <- tab[substr(tab$Security, 1, 2) == "EU", ]
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
