#class for deviations
#deviation <- setClass("deviation", slots = c(ell = "numeric", isdev = "logical", start = "numeric", end = "numeric", sign = "numeric", date = "Date"))
#setMethod("c", "deviation", function(x, y) {
#  deviation(ell = c(x@ell, y@ell), isdev = c(x@isdev, y@isdev),
#            start = c(x@start, y@start), end = c(x@end, y@end),
#            sign = c(x@sign, y@sign), date = c(x@date, y@date))
#  })

TestDeviation <- function(tab, i, ell) {
  date0 <- as.Date(tab$date[1])
  price0 <- tab$stavkaclose[i]
  indtab1 <- tab[abs(stavkaclose - price0) >= ell & id >= i, list(id = id[1], stavkaclose = stavkaclose[1]), mult = "first"]
  if (nrow(indtab1) == 0) {
    #return(deviation(ell = ell, isdev = FALSE, start = i, end = NA, sign = NA, date = date0))
    return(data.frame(ell = ell, isdev = FALSE, start = i, end = NA, sign = NA, date = date0))
  }
  price1 <- indtab1$stavkaclose[1]
  i1 <- indtab1$id[1]
  if (price1 >= price0 + ell) {
    indtab2 <- tab[abs(stavkaclose - price0 - ell) >= ell & id >= i1, list(id = id[1], stavkaclose = stavkaclose[1]), mult = "first"]
    if (nrow(indtab2) == 0) {
      return(data.frame(ell = ell, isdev = FALSE, start = i, end = NA, sign = 1, date = date0))
    }
    price2 <- indtab2$stavkaclose[1]
    i2 <- indtab2$id[1]
    if (price2 >= price0 + 2*ell) {
      return(data.frame(ell = ell, isdev = FALSE, start = i, end = i2, sign = 1, date = date0))
    }
    return(data.frame(ell = ell, isdev = TRUE, start = i, end = i2, sign = 1, date = date0))
  } else if (price1 <= price0 - ell) {
    indtab2 <- tab[abs(stavkaclose - price0 + ell) >= ell & id >= i1, list(id = id[1], stavkaclose = stavkaclose[1]), mult = "first"]
    if (nrow(indtab2) == 0) {
      return(data.frame(ell = ell, isdev = FALSE, start = i, end = NA, sign = -1, date = date0))
    }
    price2 <- indtab2$stavkaclose[1]
    i2 <- indtab2$id[1]
    if (price2 <= price0 - 2*ell) {
      return(data.frame(ell = ell, isdev = FALSE, start = i, end = i2, sign = -1, date = date0))
    }
    return(data.frame(ell = ell, isdev = TRUE, start = i, end = i2, sign = -1, date = date0))
  } else {
    stop("Wrong choice of price1!")
  }
}
UpgradeTab <- function(tab) {
  tab <- as.data.table(tab)
  tab[, stavkaclose := (stavkabid + stavkaask)/2]
  vec <- rep(NA, nrow(tab))
  dates <- unique(tab$date)
  iprev <- 1
  for (date0 in dates) {
    print(date0)
    num <- nrow(tab[date == date0, ])
    vec[iprev:(iprev + num - 1)] <- 1:num
    iprev <- iprev + num
  }
  tab[, id := vec]
}
LookForDeviations <- function(tab, ell) {
  dates <- unique(tab$date)
  res <- data.frame(ell = rep(ell, nrow(tab)), isdev = rep(FALSE, nrow(tab)),
                   start = rep(as.numeric(NA), nrow(tab)), end = rep(as.numeric(NA), nrow(tab)),
                   sign = rep(as.numeric(NA), nrow(tab)), date = rep(as.Date(NA), nrow(tab)))
  counter <- 1
  for (date0 in dates) {
    date0 <- as.Date(date0)
    print(date0)
    tabred <- tab[date == date0, ]
    setkey(tabred, "Time")
    #for (i in 1:nrow(tabred)) {
    i <- 1
    while (i <= nrow(tabred)) {
      tres <- TestDeviation(tabred, i, ell)
      if (tres$isdev) {
        res$isdev[counter] <- tres$isdev
        res$start[counter] <- tres$start
        res$end[counter] <- tres$end
        res$sign[counter] <- tres$sign
        res$date[counter] <- tres$date
        counter <- counter + tres$end - tres$start + 1
        i <- tres$end + 1
      } else {
        i <- i + 1
        counter <- counter + 1
      }
    }
  }
  res
}
GetStatInfoForDevTab <- function(devtab, dealstab, tab) {
  ell <- devtab$ell[1]
  res <- data.frame(ell = rep(as.numeric(ell), nrow(devtab)),
                    sign = rep(as.numeric(NA), nrow(devtab)), totvol = rep(as.numeric(NA), nrow(devtab)),
                    avsellprice = rep(as.numeric(NA), nrow(devtab)), avbuyprice = rep(as.numeric(NA), nrow(devtab)))
  setkey(tab, "Date", "id")
  setkey(dealstab, "Time")
  for (i in 1:nrow(devtab)) {
    if (i %% 1000 == 0) {print(i)}
    start <- devtab$start[i]
    end <- devtab$end[i]
    date0 <- as.Date(devtab$date[i])
    timestart <- tab[Date == date0 & id == start, Time]
    timeend <- tab[Date == date0 & id == end, Time]
    dealstabred <- dealstab[Time >= timestart & Time <= timeend, list(volume = abs(Volume), price = diff/1000, side = Side)]
    if (nrow(dealstabred)>0) {
      res$sign[i] <- devtab$sign[i]
      res$totvol[i] <- dealstabred[, sum(volume)]
      gtab <- dealstabred[, list(val = sum(volume*price)/sum(volume)), by = "side"]
      val <- gtab[side == "Sell", val]
      if (length(val)>0) {res$avsellprice[i] <- val}
      val <- gtab[side == "Buy", val]
      if (length(val)>0) {res$avbuyprice[i] <- val}
    }
  }
res
}
PlotDoubleStatInfo <- function(statinfo) {
  statinfoplus <- na.omit(statinfo[statinfo$sign > 0, ])
  statinfominus <- na.omit(statinfo[statinfo$sign < 0, ])
  layout(matrix(1:4, nrow = 2))
  truehist(log(statinfoplus$totvol))
  truehist(log(statinfominus$totvol))
  truehist(statinfoplus$avbuyprice - statinfoplus$avsellprice)
  truehist(statinfominus$avbuyprice - statinfominus$avsellprice)
}
