MainStrategy <- function(tabb, size, shift=NA, delay=NA, idnum=1, rent=0, mode="odd", uplim=25, N, control, index)
{
	tabb <- tabb[index, ]
	resdata <- data.frame(v1=c(), v2=c(), v3=c(), v4=c(), v5=c(), v6=c())
	for (periodnum in unique(tabb$periodnum))
	{
		tab <- tabb[tabb$periodnum == periodnum,]
		days <- unique(tab$day)

		for (day in days)
		{
			tabday <- tab[tab$day == day,]

			datecut <- 15*60
			datemin <- tabday$date[1]
			datemax <- tabday$date[length(tabday$date)]
			tabday <- tabday[(tabday$date>datemin + datecut) & (tabday$date<datemax - datecut),]
			
			if (dim(tabday)[1]<400) {next}
			resdata <- rbind(resdata, c(AnalyzeDay(tab=tabday, size=size, shift=shift, delay=delay, idnum=idnum, rent=rent, uplim=uplim, N=N, control=control),day))

		}
	}
resdata
}

AnalyzeDay <- function(tab, size, shift=NA, delay=NA, idnum=1, rent=0, uplim=25, N=0, control)
{
print("NEW")
if (idnum==9)
{
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy9(tab, size, size0, shift, shift0, delay, N, rent, uplim), sdprev, sdcur)
}
resdata
}

