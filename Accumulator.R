#assumptions
#columns: si.Time, si.Volume, si.Direction, stavka
#here the volume can be less than 0
#to get deals use GetStavkaDataForTrader from DollarStavkaGraphs.R
AccumulateTrades <- function(deals, vol) {
  deals <- as.data.table(as.data.frame(deals))
  nextval <- 0
  startindex <- 1
  curpos <- 0
  accumulateddeals <- data.frame(Time = character(), stavka = numeric(),
                                 si.Direction = character(), si.Volume = numeric())
  for (j in 1:nrow(deals)) {
    #curpos <- curpos + ifelse(deals$si.Direction[j] == "Buy", deals$si.Volume[j], -deals$si.Volume[j])
    curpos <-  curpos + deals$si.Volume[j]
    flag <- FALSE
    if (curpos >= nextval + vol) {
      flag <- TRUE
      nextval <- nextval + vol
    } else if (curpos <= nextval - vol) {
      flag <- TRUE
      nextval <- nextval - vol
    }
    if (flag) {
      print(c(nextval, startindex, j))
      price <- deals[startindex:(j), sum(stavka*abs(si.Volume))/sum(abs(si.Volume))]
      startindex <- j + 1
      accumulateddeals <- rbind(accumulateddeals,
                                data.frame(
                                  Time = deals$si.Time[j],
                                  stavka = deals$stavka[j],
                                  si.Direction = deals$si.Direction[j],
                                  si.Volume = ifelse(deals$si.Direction[j] == "Buy", vol, -vol)
                                ))
    }
  }
  accumulateddeals <- as.data.table(accumulateddeals)
  accumulateddeals[, si.Date := as.Date(Time)]
  accumulateddeals[, si.Time := Time]
accumulateddeals
}
