# there is some functions which print result of backtesting to file, string etc...


# quik view of main simulation parameters. old function has to be refactor. return string
print.stat <- function(stat) {
	paste0(
		stat$instrument, "   ",
		stat$candle.type, ": ",
		" | ",as.character(do.call(what=paste, args=c(as.list(stat$args), sep=","))), " | ",
		"pnl: ", stat$total.pl.rub, " [year%: ", round(100 * stat$year.return), "] ",
		"dr: ", stat$max.drawdown.rub, " [#trades: ", stat$n.of.trades, "]"
	)
}

# the idea was "lets save all simulation result to folder"
save.backtest <- function(stat, folder.path=file.path("..","..","Results")) {
	dir.create(folder.path, showWarnings=FALSE)
	wd <- file.path(folder.path, Sys.time())
	dir.create(wd)

	saveRDS(stat,file.path(wd, "stat.RDS"))

	write.csv(file=file.path(wd, "trades.csv"), stat$trades, quote=F, eol="\n", row.names=F)
	write.csv(file=file.path(wd, "data.csv"), stat$df, quote=F, eol="\n", row.names=F)
}


# print.stat() --> "result.txt"
append.stat <- function(stat, file=file.path("..","..","Results","result.txt"), append=T) {
	if(!file.exists(file))
		file.create(file, showWarnings=F)
  write.csv(print.stat(stat), file, quote=F, eol="\n", append=append, sep=";")
}
