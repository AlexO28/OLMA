# TO DO add description
threshold.levels <- function(df, levels, treshold) {
  buy.mask <- levels <= (-treshold)
  sell.mask <- levels >= treshold
  levels[buy.mask] <- 1
  levels[sell.mask] <- -1
  levels[is.na(levels) | abs(levels) != 1] <- 0
  levels[levels == c(0, levels[-length(levels)])] <- 0
  levels
}

threshold.indicator <- function(df, indicator, treshold, ...) {
  levels = indicator(...)
  threshold.levels(df, levels, treshold)
}
