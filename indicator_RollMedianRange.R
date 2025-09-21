# median of range candle
RollMedianRange <- function(df, n) {
  range <- df$High - df$Low
  RollMedian(range, n)
}