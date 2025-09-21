options(stringsAsFactors = FALSE)
options(digits.secs = 3)
futname <- "MMH6@FORTS"
fromwar <- TRUE

Main <- function() {
  alpha <- 1.587214
  info <- read.table("\\\\192.168.1.204\\share\\People\\Алексей\\CalculateBetaByDeals\\inparams.txt",
                     header = FALSE, sep = "=")
  filepath <- as.character(info[info[, 1] == "filepath", 2])
  print(filepath)
  datestart <- info[info[, 1] == "datestart", 2]
  print(datestart)
  dateend <- info[info[, 1] == "dateend", 2]
  print(dateend)
  tab <- read.table(paste0(filepath), header = TRUE, sep = ";", row.names = NULL, dec = ",")
  print(head(tab))
  tab <- data.frame(LocalTime = tab[, 3], Security = tab[, 4], Volume = tab[, 5],
                    Price = tab[, 6], Direction = tab[, 7])
  groupedtab <- GroupMMVBData(tab, datestart = datestart, dateend = dateend)
  groupedtab2 <- GroupRISIData(groupedtab)
  groupedtab2$beta <- groupedtab2$Price - alpha*0.00002*groupedtab2$PriceRI*groupedtab2$PriceSI
  atime <- Sys.time()
  atime <- gsub(" ", "_", as.character(atime))
  atime <- gsub(":", "-", atime)
  myfile <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CalculateBetaByDeals\\groupeddeals_", atime, ".csv")
  print(head(groupedtab2))
  #groupedtab2 <- apply(groupedtab2, 2, as.character)
  #print(head(groupedtab2))
  write.table(groupedtab2, myfile, sep = ";", row.names = FALSE)
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
GroupMMVBData <- function(tab, raw = TRUE, datestart = "2015-01-01", dateend = "2115-01-01") {
  if (raw) {
    names(tab) <- tolower(names(tab))
    print(head(tab))
    tab$price[tab$security == futname] <- 100*tab$price[tab$security == futname]
    tab$direction <- ifelse(tab$direction == "Buy", 1, -1)
    if (fromwar) {
      tab$tradetime <- as.POSIXct(tab$localtime, format = "%d-%m-%Y %H:%M:%OS")
    } else {
      tab$tradetime <- as.POSIXct(tab$tradetime, format = "%d-%m-%Y %H:%M:%OS")
    }
    tab$time <- NULL
    tab$time <- tab$tradetime
    tab$date <- as.Date(tab$time)
    tab <- tab[tab$date >= as.Date(datestart) & tab$date <= as.Date(dateend), ]
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
