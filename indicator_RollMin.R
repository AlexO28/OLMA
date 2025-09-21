#
#
RollMin <- function(x, n, partial = TRUE) {
  rollapply(x, width=n, FUN=min, align="right", partial = partial, fill=NA)
}