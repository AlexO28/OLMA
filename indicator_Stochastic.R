# stochastic indicator
# https://en.wikipedia.org/wiki/Stochastic_oscillator

Stochastic.K <- function(df, n) {
  high <- RollMax(df$High, n, partial = TRUE)
  low <- RollMin(df$Low, n, partial = TRUE)
  k <- 100 * (df$Close - low) / (high - low)
}


Stochastic.D <- function(k, n2) {
  RollMean(k, n2)
}

