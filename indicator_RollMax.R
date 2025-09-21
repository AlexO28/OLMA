RollMax <- function(x, n, partial = TRUE) {
  rollapply(x, width=n, FUN=max, align="right", partial = partial, fill=NA)
}