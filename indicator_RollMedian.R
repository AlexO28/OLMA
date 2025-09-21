# if partial == FALSE, NA while i<n
RollMedian <- function(x, n, partial=TRUE) {
	rollapply(x, width=n, FUN=median, align="right", partial = partial, fill=NA)
}