
RollCorCoeff <- function(x1, x2, n, method = "kendall") {
  corr <- rep(x = NA, times = n)
  for(i in (n+1):length(x)) {
    v1 <- x1[(i-n):(i-1)]
    v2 <- x2[(i-n):(i-1)]
    corr[i] <- cor(v1, v2, method = method)
  }
  corr
}