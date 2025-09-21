#len is a size of a slipping window
Purify <- function(tab, len, strname) {
  strname1 <- paste0(strname, "bid")
  strname2 <- paste0(strname, "ask")
  index <- (len + 1):nrow(tab)
  indexprev <- 1:((nrow(tab) - len))
  inds <- which((tab[index, strname1] == tab[indexprev, strname1]) & (tab[index, strname2] == tab[indexprev, strname2]))
  print("roughly")
  print(length(inds))
  resinds <- rep(NA, length(inds))
  for (i in seq_along(inds)) {
    ind <- indexprev[i]
    if ((i %% 10000 == 0)) {print(i)}
    resinds[i] <- 1
    if (ind+len-1 > nrow(tab)) {break}
    for (k in (ind+1):(ind+len-1)) {
      if ((tab[k, strname1] == tab[ind, strname1]) & (tab[k, strname2] == tab[ind, strname2])) {
      } else {resinds[i] <- NA; break}
    }
  }
inds[!is.na(resinds)]
}
Purify2 <- function(tab, len, strname) {
  strname1 <- paste0(strname, "bid")
  strname2 <- paste0(strname, "ask")
  print(strname1)
  print(strname2)

  mymax1 <- rollmaxr(tab[, strname1], len, FALSE)
  mymin1 <- -rollmaxr(-tab[, strname1], len, FALSE)
  mymax2 <- rollmaxr(tab[, strname2], len, FALSE)
  mymin2 <- -rollmaxr(-tab[, strname2], len, FALSE)
  signals <- (mymax1 == mymin1) & (mymax2 == mymin2)
  signals[1:len] <- FALSE
which(signals)
}
KillQuantiles <- function(tab, alpha, strname, len) {
  vec <- rollapplyr(tab[, strname], len, mean, partial = TRUE)
  diff <- tab[, strname] - vec
  quant1 <- as.numeric(quantile(diff, alpha))
  quant2 <- as.numeric(quantile(diff, 1-alpha))
tab[(diff >= quant1) & (diff <= quant2), ]
}
GetPureData <- function(instname = instname0, startdate = startdate0, enddate = enddate0) {
  tab <- LoadBp(instrument = instname, start.date = startdate, end.date = enddate,
                candle.type = "1s", storage.path = "D:")
  mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"), tz = "GMT")
  tab <- as.data.frame(na.omit(tab))
  tab <- tab[mask > 0, ]
  names(tab) <- tolower(names(tab))
  print(head(tab))

  ind <- Purify2(tab, len = 900, strname = "")
  if (length(ind) > 0) {tab2 <- tab[-ind, ]} else {tab2 <- tab}
  htab <- aggregate(tab2[, 1], by = list(tab2$date), FUN = length)
  tab2 <- data.table(tab2[!(tab2$date %in% htab[htab[, 2] <= 20000, 1]), ])
  htab <- aggregate(tab2$date, by = list(tab2$date), FUN = length)
  list(tab = tab2, htab = htab)
}
