RollSd <- function(x, n, partial = FALSE) {
  rollapply(x, width=n, FUN=sd, align="right", partial = partial, fill=NA)
}