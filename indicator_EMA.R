# exponential moving average
# https://en.wikipedia.org/wiki/Moving_average

EMA.a <- function(x, alpha) {
  ema <- x
  for(i in 2:length(x))
    ema[i] <- (1 - alpha) * ema[i-1] + alpha * x[i]
  ema
}

EMA <- function(x, n, partial = FALSE) {
  alpha = 2 / (n + 1)
  ema <- x
  ema[1:n] <- cumsum(x[1:n])/(1:n)
  for(i in (n+1):length(x))
    ema[i] <- (1 - alpha) * ema[i-1] + alpha * x[i]
  if(!partial)
    ema[1:n] <- NA
  ema
}