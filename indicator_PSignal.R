ind.psignal <- function(df, npoints=4) {
	dif.price <- c(NA, diff((df$Close+df$Open)/2))
	range <- df$High-df$Low
	nintr <- npoints-1
	mean.dif <- RollMean(dif.price, nintr)
	disp.dif <- RollVar(dif.price, nintr)
	omega <- RollMean(range,nintr)
	psignal <- sign(mean.dif)/(1+abs(disp.dif/(omega*mean.dif)))
	psignal
}

ind.psignal.avr <- function(df, nsma=3, npoints=4) {
  x = RollMean(ind.psignal(df,npoints), n=nsma)
  x[is.na(x) | is.nan(x)] <- 0
  x
}
