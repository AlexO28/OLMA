RandomDates <- function(dates, ratio=0.5) {
	inds = seq_along(dates)
	train.inds = sample(inds, ratio*length(dates))
	list(
		train=dates[train.inds],
		test =dates[-train.inds])
}

RandomSplit <- function(df, ratio=0.7) {
	dates = RandomDates(unique(df$Date), ratio=ratio)
	list(
		df.train = df[df$Date %in% dates$train, ],
		df.test = df[df$Date %in% dates$test, ])
}
