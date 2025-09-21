# median of range candle
RollMedianRange <- function(df, n) {
  range <- df$High - df$Low
  RollMedian(range, n)
}
# mean of range candle
RollMeanRange <- function(df, n) {
	range <- df$High - df$Low
	RollMean(range, n)
}

# Range of several candles

RollRange <- function(df, n, partial = TRUE) {
		max = RollMax(df$High, n, partial)
		min = RollMin(df$Low, n, partial)
		max - min
}
