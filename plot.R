####I suggest to erase everything after this line!

plot.data <- function(df, trades = NULL, ...) {

}

add.ind <- function(plot.obj, ind) {

}

plot.test <- function(df, trades, origin=origin, ...) {
	trades$time <- as.POSIXct(trades$time, origin=origin)
	ggplot(data=df, aes(x=Time, y=Close)) +
		geom_line() +
		geom_point(data=trades, aes(x=time, y=price, colour=as.factor(dir)))
  #plot df with trades on it
  #plot equity curve
  #plot draw down curve
  #barplot for daily return
}

plot.stat <- function(stat, type="rel") {
  plot(stat$equity.rel,
       xlab="",
       ylab="",
       sub=paste0(stat$instrument,"-",stat$candle.type," [no commis]"),
       type="l",
       lwd=2)
  abline(h = 0)
}

###I do not use here the indicator, since we do not have one format for indicators. Unfortunately.
###Please confer RollMean and ParabolicSD.
plot.trades <- function(df, trades, indicator, day) {

  df$ind <- indicator
  df <- df[is.na(df$ind) == FALSE, ]
  if (is.null(df$Bid)) {
    df$Bid <- df$Close
    df$Ask <- df$Close
  }
  if (!is.na(day)) {
    df <- df[as.Date(as.POSIXct(df$Time, origin = "1970-01-01")) == day, ]
    trades <- trades[as.Date(as.POSIXct(trades$time, origin = "1970-01-01")) == day, ]
  }
  par(mfcol = c(2,1))
  plot(as.POSIXct(df$Time, origin = "1970-01-01"), df$Bid, type = "l")
  lines(as.POSIXct(df$Time, origin = "1970-01-01"), df$Ask, col = "blue")

  for (j in 1:nrow(trades)) {
 #   abline(v=as.POSIXct(trades$time[j], origin = "1970-01-01"))
    if (trades$dir[j] > 0) {
       points(as.POSIXct(trades$time[j], origin = "1970-01-01"), trades$price[j],pch=24, col="green", cex=1)
    } else if (trades$dir[j] < 0) {
      points(as.POSIXct(trades$time[j], origin = "1970-01-01"), trades$price[j],pch=25, col="red", cex=1)
    }
  }
  plot(as.POSIXct(df$Time, origin = "1970-01-01"), df$ind, type="l", col = "red")
  for (j in 1:nrow(trades)) {
    abline(v=as.POSIXct(trades$time[j], origin = "1970-01-01"))
  }
#  abline(h=90, col = "black")
#  abline(h=10, col = "black")
}
plot.trades2 <- function(df, trades, ind1, ind2, day) {
  if (!is.na(day)) {
    df$ind1 <- ind1
    df$ind2 <- ind2
    df <- df[as.Date(df$Time) == as.Date(day), ]
    trades <- trades[as.Date(trades$time) == as.Date(day), ]
    ind1 <- df$ind1
    ind2 <- df$ind2
  }

  df <- df[is.na(ind1) == FALSE, ]
  ind1 <- ind1[is.na(ind1) == FALSE]
  ind2 <- ind2[is.na(ind2) == FALSE]

  plot(as.POSIXct(df$Time, origin = "1970-01-01"), df$Close, type = "l")
  lines(as.POSIXct(df$Time, origin = "1970-01-01"), ind1, col = "blue")
  lines(as.POSIXct(df$Time, origin = "1970-01-01"), ind2, col = "blue")
  for (j in 1:nrow(trades)) {
    if (trades$dir[j] > 0) {
      points(trades$time[j], trades$price[j], pch = 24, col = "green", cex = 1)
    } else {
      points(trades$time[j], trades$price[j], pch = 25, col = "red", cex = 1)
    }
  }
}
###df --- data.frame with column Close
###ind --- an indicator, i.e. a vector
plot.dfwithind <- function(df, ind, lower, upper) {
  fun <- function(v) {
    abline(v = v, col = "red")
  }
  vec <- which(((ind<lower) | (ind>upper)))
  ####print(ind[vec])
  par(mfcol = c(2,1))
  plot(df$Time, df$Close, type = "l")
  vecred <- rbind(df$Time[vec], c())
  apply(vecred, 1, fun)
  plot(df$Time, ind, type = "l")
  apply(vecred, 1, fun)
}
