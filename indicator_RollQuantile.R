RollQuantile <- function(x, alpha, n, partial = FALSE) {
 quantmod <- function(x) {
    quantile(x, alpha)
 }
 rollapply(x, width = n, FUN = quantmod, align="right",partial = partial, fill=NA)
}
