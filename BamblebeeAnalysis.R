GetSubset <- function(tab1, tab2, tab3, starttime, endtime) {
  tab1$Time <- as.POSIXct(tab1$Time, origin = "1970-01-01")
  tab2$Time <- as.POSIXct(tab2$Time, origin = "1970-01-01")
  tab3$Time <- as.POSIXct(tab3$Time, origin = "1970-01-01")
  tab1 <- tab1[(tab1$Time >= starttime) & (tab1$Time <= endtime), ]
  tab2 <- tab2[(tab2$Time >= starttime) & (tab2$Time <= endtime), ]
  tab3 <- tab3[(tab3$Time >= starttime) & (tab3$Time <= endtime), ]
cbind(tab1, tab2, tab3)
}
GetModel <- function(v1, v2) {
  1.588*0.00002*v1*v2
}
PlotModelWithTime <- function(times, bids, asks, atime) {
  YLIM <- c(min(bids), max(asks))
  plot(times, bids, col = "green", type = "l", ylim = YLIM)
  lines(times, (bids + asks)/2, col = "blue")
  lines(times, asks, col = "red")
  abline(v = as.POSIXct(atime))
}
EvaluatePortfolio <- function(vol1, price1, vol2, price2) {
  vol1*price1 + vol2*price2*1.09
}
GetHedgeNum <- function(vol1, price1, price2) {
  -vol1*price1/(price2*1.09)
}