#MACD - diff(EMA1,EMA2)
#https://en.wikipedia.org/wiki/MACD

MACD.hist <- function(df, fast.period, slow.period, partial = TRUE) {
  EMA(df$Close, n = fast.period, partial = partial) - EMA(df$Close, n = slow.period, partial = partial) 
}

MACD.signal <- function(MACD, signal.period) {
  RollMean(MACD, signal.period, partial = TRUE)
}