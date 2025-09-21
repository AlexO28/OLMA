###на вход подаются данные за ceiling(L) дней по ММВБ, РТС и доллару, а также данные за текущий день.
###выход: дата-фрейм со структурой time, alpha, beta
MMVBRegressionSliding <- function(tabMMVB, tabRTS, tabDOL, tabMMVBcur, tabRTScur, tabDOLcur, L, S, K = 0.00002, candletype) {
  if (candletype == "1m") {
    err <- 60
  } else {
    err <- 1
  }
  numofhours <- nrow(tabMMVB)/((ceiling(L))*60*60/err)
  print(numofhours)
  if (S > 1) {
    stop("this regime is not supported!")
  } else if (S == 1) {
    dates <- c(tabMMVBcur$Time[1])
  } else {
    dates <- seq(as.POSIXct(tabMMVBcur$Time[1], origin = "1970-01-01", tz = "UTC"),
                 as.POSIXct(tabMMVBcur$Time[nrow(tabMMVBcur)] + err, origin = "1970-01-01", tz = "UTC"),
                 S*numofhours*60*60)
    print(dates)
###    dates <- dates[dates %in% tabMMVBcur$Time]
###    print(dates)
  }
  largetab <- data.frame(Time = c(tabMMVB$Time, tabMMVBcur$Time), vec1 = c(tabMMVB$Close, tabMMVBcur$Close),
                         vec2 = c(K * tabRTS$Close * tabDOL$Close, K * tabRTScur$Close * tabDOLcur$Close))
  res <- sapply(dates, GetKefsDays, df = largetab, L = L, err = err, numofhours = numofhours)
  res <- as.data.frame(t(res))
  names(res) <- c("time", "beta", "alpha")
  res$time <- as.POSIXct(res$time, origin = "1970-01-01", tz = "UTC")
res
}
###разница между GetKefsDays и GetKefs по сути только в маштабе
GetKefsDays <- function(adate, df, L, err, numofhours) {
  df <- df[df$Time <= adate, ]
  ind <- which(df$Time == adate)
  if (length(ind) == 0) {
    ind <- nrow(df)
  }
  dfred <- df[(ind-1):(ind - floor(L*numofhours*60*60/err)), ]
  kefs <- as.numeric(lm(dfred$vec1 ~ dfred$vec2)$coefficients)
  rbind(c(), c(adate, kefs))
}
###на вход подаются данные за N-дней по ММВБ, РТС и доллару и текущая дата
###выход: дата-фрейм со структурой time, alpha, beta
MMVBRegression <- function(tabMMVB, tabRTS, tabDOL, adate, K = 0.00002) {
  vec1 <- tabMMVB$Close
  vec2 <- (K * tabRTS$Close) * tabDOL$Close
  kefs <- as.numeric(lm(vec1 ~ vec2)$coefficients)
  adate <- as.POSIXct(paste(as.character(adate), "10:00:00"), origin = "1970-01-01", tz = "UTC")
data.frame(time = adate, beta = kefs[1], alpha = kefs[2])
}
###на входе подаются данные за 1 день и период нарезки в секундах
###на выходе дата-фрейм со структурой time, alpha, beta
MMVBRollingRegression <- function(tabMMVB, tabRTS, tabDOL, L, S,  K = 0.00002, candletype) {
  if (candletype == "1m") {
    err <- 60
  } else {
    err <- 1
  }
  dates <- seq(as.POSIXct(tabMMVB$Time[1], origin = "1970-01-01", tz = "UTC") + L*err + err,
               as.POSIXct(tabMMVB$Time[nrow(tabMMVB)], origin = "1970-01-01", tz = "UTC"), S*err)
  dates <- dates[dates %in% tabMMVB$Time]

  largetab <- data.frame(Time = tabMMVB$Time, vec1 = tabMMVB$Close, vec2 = K * tabRTS$Close * tabDOL$Close)
  res <- sapply(dates, GetKefs, df = largetab, L = L)
  res <- as.data.frame(t(res))
  names(res) <- c("time", "beta", "alpha")
  res$time <- as.POSIXct(res$time, origin = "1970-01-01", tz = "UTC")
res
}

GetKefs <- function(adate, df, L) {
  ind <- which(df$Time == adate)
  dfred <- df[(ind-1):(ind-L), ]
  kefs <- as.numeric(lm(dfred$vec1 ~ dfred$vec2)$coefficients)
  rbind(c(), c(adate, kefs))
}

MMVBRollingModel <- function(tabMMVB, tabRTS, tabDOL, L, S, K = 0.00002) {
  kefs <- MMVBRollingRegression(tabMMVB, tabRTS, tabDOL, L, S, K)
###  print(summary(kefs))
  tabMMVB <- tabMMVB[tabMMVB$Time >= kefs$time[1], ]
  tabRTS <- tabRTS[tabRTS$Time >= kefs$time[1], ]
  tabDOL <- tabDOL[tabDOL$Time >= kefs$time[1], ]
  tabMMVB <- merge(tabMMVB, kefs, by.x = "Time", by.y = "time", all.x = TRUE)
  lastalpha <- tabMMVB$alpha[1]
  lastbeta <- tabMMVB$beta[1]

  for (i in 1:nrow(tabMMVB)) {
    if (is.na(tabMMVB$beta[i])) {
      tabMMVB$beta[i] <- lastbeta
      tabMMVB$alpha[i] <- lastalpha
    } else {
      lastbeta <- tabMMVB$beta[i]
      lastalpha <- tabMMVB$alpha[i]
    }
  }
  tabMMVB$model <- 0
  vec <- tabRTS$Close * tabDOL$Close
  vecmod <- c(NA, vec[1:(length(vec)-1)])

  tabMMVB$model <- K*vecmod*tabMMVB$alpha + tabMMVB$beta
  tabMMVB <- tabMMVB[!is.na(tabMMVB$model),]

  kefs$alpha[is.na(kefs$alpha)] <- 0
  kefs$alpha[kefs$alpha < 0] <- 0
  kefs$alphaprev <- c(NA, kefs$alpha[1:(nrow(kefs)-1)])
  kefs$betaprev <- c(NA, kefs$beta[1:(nrow(kefs)-1)])
  kefs <- kefs[!is.na(kefs$alphaprev),]

  temptab <- data.frame(Time = tabRTS$Time, vec = K * tabRTS$Close * tabDOL$Close)
  temptab <- merge(kefs, temptab, by.x = "time", by.y = "Time")
  kefs <- data.frame(time = temptab$time, model1 = temptab$alpha*temptab$vec + temptab$beta,
                     model2 = temptab$alphaprev*temptab$vec + temptab$betaprev, alpha = 0.001*kefs$alpha, alphaprev = 0.001*kefs$alphaprev)
list(tab = data.frame(Time = as.POSIXct(tabMMVB$Time), Close = tabMMVB$Close, model = tabMMVB$model), kefs = kefs)
}
