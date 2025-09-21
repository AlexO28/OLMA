RollVar <- function(x,n) {
	rollapply(x, width=n, FUN=var, align="right", fill=NA)
}