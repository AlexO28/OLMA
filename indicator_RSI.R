# RSI
# https://en.wikipedia.org/wiki/Relative_strength_index

.RSI.x <- function(x, n) {
  dif <- c(0,diff(x))
  U <- dif
  U[dif <= 0] <- 0
  D <- dif
  D[dif >= 0] <- 0
  RS  <- EMA(U, n, partial = TRUE) / EMA(abs(D), n, partial = TRUE)
  RSI <- 100 - 100 / (1 + RS)
}

RSI <- function(df, n) {
  .RSI.x(df$Close, n)
}
