
# remove one-point-gaps (except)
clean.df <- function(df, treshold) {
	dif <- c(0, diff(df$Close))
	dif.next <- c(dif[-1], 0)
	mask <- (dif >= treshold & dif.next <= -treshold) |
		(dif <= -treshold & dif.next >= treshold)
	w <- which(mask)
	time.start = (hour(df$Time)==10 & minute(df$Time)==0)
	for(i in w) {
		if(!time.start[i]) {
			df$Close[i] <- df$Close[i-1]
		}
	}
	# теперь для двух
	n = nrow(df)
	cl = df$Close
	mask = rep(F,n)
	up.gap <- which(dif >= treshold)
	for(i in up.gap)
	{
		if(i<=(n-2) & !time.start[i]){
			if((cl[i+1]-cl[i+2])>treshold) {
				mask[i]<-T
				mask[i+1]<-T
				cl[i]<-cl[i-1]
				cl[i+1]<-cl[i-1]
			}
		}
	}

	down.gap <- which(dif <= -treshold)
	for(i in down.gap)
	{
		if(i<=(n-2) & !time.start[i]){
			if((cl[i+1]-cl[i+2])< -treshold) {
				mask[i]<-T
				mask[i+1]<-T
				cl[i]<-cl[i-1]
				cl[i+1]<-cl[i-1]
			}
		}
	}

	df$Close =cl
	df
}

DangerousDates <- c(seq(as.Date("2014-03-11"), as.Date("2014-03-17")+7, 1), seq(as.Date("2014-06-10"), as.Date("2014-06-16")+7, 1),
										seq(as.Date("2014-09-09"), as.Date("2014-09-15")+7, 1), seq(as.Date("2014-12-09"), as.Date("2014-12-15")+7, 1),
										seq(as.Date("2015-03-10"), as.Date("2015-03-16")+7, 1))

BadDates <- c(seq(as.Date("2014-03-11"), as.Date("2014-03-17"), 1), seq(as.Date("2014-06-10"), as.Date("2014-06-16"), 1),
							seq(as.Date("2014-09-09"), as.Date("2014-09-15"), 1), seq(as.Date("2014-12-09"), as.Date("2014-12-15"), 1),
							seq(as.Date("2015-03-10"), as.Date("2015-03-16"), 1))


###эта функция берёт все файлы из каталога, делает даты из названия, и делит выборку на две части
# "C:\\Users\\user\\Desktop\\123456\\1s\\SPFB.MIX@FORTS\\"
RandomSplitFiles <- function(dir, ratio, mode, dates) {
	files <-  list.files(dir)

	dates <- sapply(files, StrToDate) %in% dates

	print(dates[1:10])
	print(length(dates))

	dates <- as.Date(dates, origin=origin)

	names(dates) <- c()
	dates <- dates[!is.na(dates)]
	dates <- as.Date(dates, origin = "1970-01-01")

	if (mode == 1) {
		dates <- dates[!(dates %in% BadDates)]
	} else {
		dates <- dates[!(dates %in% DangerousDates)]
		dates <- dates[dates >= as.Date("2014-01-15")]
	}
	inds <- 1:length(dates)
	train.inds <- sample(inds, ratio*length(dates))
	test.inds <- inds[!(inds %in% train.inds)]
	list(traindates = dates[train.inds], testdates = dates[test.inds])
}

StrToDate <- function(str) {
	####  print(substr(str, 49,58))
	as.Date(substr(str, 49,58), format = "%Y_%m_%d", origin = "1970-01-01")
}
