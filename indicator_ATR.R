# ATR = average true range, rollvar in L1-norm
#

.true.range <- function(df) {
  h = df$High
  l = df$Low
  close <- c(df$Close[1], df$Close[-nrow(df)])
  pmax((h - l), abs(h - close), abs(l - close))
}

ATR <- function(df, n) {
  RollMean(.true.range(df), n, partial = TRUE)
}