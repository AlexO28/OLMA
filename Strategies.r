###требования к таблице
###дата-фрейм со столбцами
###$day --- номер дня (первый день 0 --- исключается из рассмотрения из-за неполноты данных)
###$date --- дата внутри дня (число от 0 до 86400)
###$bid
###$ask

nnstrat <- 9

###требуются таблицы mytab1, ..., mytab7
###требуется инициализация глобальных переменных с рекоммендуемыми значениями:
###withlog <- FALSE
###strategycontrol <- "FULL"

#library(RPostgreSQL)
###процедура получения данных по ставкам
###вход: номер стратегии, параметры стратегии
###выход: дата-фрейм вида: $sign, $quantity, $day, $date
GetDealsData <- function()
{
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname="postgres", user="postgres", password="123")
	str <- "select * from \"Deals\" where \"nnstrat\"=7 order by \"day\", \"date\""
	tab <- dbGetQuery(con,str)
	tab
}
###процедура рисования графика со ставками
###вход: номер дня, табличка со ставками
DrawBetsGraph <- function(day, deals, tab)
{
	dealsred <- deals[deals$day == day,]
	if (dim(dealsred)[1] == 0) {print("No deals in that day!")}
	else
	{
		tabred <- tab[tab$day == day,]
		plot(tabred$date, pmin(tabred$bid, tabred$ask), type="l")
		lines(tabred$date, pmax(tabred$bid, tabred$ask), col="blue")
		for (j in 1:dim(dealsred)[1])
		{
			if (dealsred$sign[j]>0)
			{
				points(dealsred$date[j], dealsred$quantity[j], pch=24, col="green", cex=3, gp=gpar(fill="green",col="green"))
			}
			else if (dealsred$sign[j]<0)
			{
				points(dealsred$date[j], dealsred$quantity[j], pch=25, col="red", cex=3, gp=gpar(fill="red",col="red"))
			}
		}
	}

}

###функция получения данных
GetData <- function()
{
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname="postgres", user="postgres", password="123")
	####str <- "select \"Daynum\" as day, \"Mytime\" as date, \"bid0\" as ask, \"ask0\" as bid, \"ExpDays\" as ExpDays, \"PeriodNum\" as PeriodNum, \"Date0\" as date0 from \"StavkaValuta\" where \"bid0\" is not null order by \"Daynum\" asc, \"Mytime\" asc"
	str <- "select A1.\"Daynum\" as day, A1.\"Mytime\" as date, A2.\"val\"/1000 - A1.\"val\" as price1, (A2.\"val\"/1000 - A1.\"val\")/A1.\"val\" as price2, A1.\"date\" as date0, A1.\"ExpDays\" as ExpDays, A1.\"PeriodNum\" as PeriodNum  from \"FinamData\" as A1 inner join \"FinamData\" as A2 on A1.\"date\"=A2.\"date\" and A1.\"time\"=A2.\"time\" where A1.\"name\"='USD000UTSTOM' and A2.\"name\"='SPFB.Si' order by date0, date"

	print(str)

	tab <- dbGetQuery(con,str)
	tab
}
###процедура тестирования на периодах
###MainStrategy <- function(tab, size, shift=NA, delay=NA, idnum=1, rent=0, mode="odd", uplim=25, N, control)
PeriodTest <- function(size, shift, delay, N, control1, control2, rent, shift0=shift, size0=size)
{
datum<- c()
print("new")
	if (control1==1)
	{
		val <- c()
		datum <- rbind(c(),c())
		res1 <- MainStrategy(mytab1, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab1, size, shift, delay, 7, rent, "even", 1, N,control2)
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))
		res1 <- MainStrategy(mytab2, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab2, size, shift, delay, 7, rent, "even", 1, N,control2)	
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))
		res1 <- MainStrategy(mytab3, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab3, size, shift, delay, 7, rent, "even", 1, N,control2)
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))
		res1 <- MainStrategy(mytab4, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab4, size, shift, delay, 7, rent, "even", 1, N,control2)
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))
		res1 <- MainStrategy(mytab5, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab5, size, shift, delay, 7, rent, "even", 1, N,control2)
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))	
		res1 <- MainStrategy(mytab6, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab6, size, shift, delay, 7, rent, "even", 1, N,control2)
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))
		res1 <- MainStrategy(mytab7, size, shift, delay, 7, rent, "odd", 1, N,control2)
		res2 <- MainStrategy(mytab7, size, shift, delay, 7, rent, "even", 1, N,control2)
		val <- c(val, sum(res1[,2]) + sum(res2[,2]))

	}
	else if (control1==0)
	{
		datum <- rbind(c(),c())
		val <- c()
		res <- ExtraDayMainStrategy(mytab1, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(1,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), sum(res$transfer)))
		val <- c(val, res$cash)
		res <- ExtraDayMainStrategy(mytab2, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(2,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)),sum(res$transfer)))
		val <- c(val, res$cash)

		print(c(length(res$cashstate), length(res$deals), length(res$currentstate)))

		res <- ExtraDayMainStrategy(mytab3, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(3,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)),sum(res$transfer)))
		val <- c(val, res$cash)
		res <- ExtraDayMainStrategy(mytab4, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(4,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)),sum(res$transfer)))
		val <- c(val, res$cash)
		res <- ExtraDayMainStrategy(mytab5, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(5,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)),sum(res$transfer)))
		val <- c(val, res$cash)
		res <- ExtraDayMainStrategy(mytab6, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(6,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)),sum(res$transfer)))
		val <- c(val, res$cash)
		res <- ExtraDayMainStrategy(mytab7, c(size, shift, shift0, size0), rent, control=control2)
		datum <- rbind(datum, cbind(7,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)),sum(res$transfer)))
		val <- c(val, res$cash)
	}
list(val=val, datum=datum)
}
###процедура работы с внедневными стратегиями на тестовой выборке
CheckStrat <- function(size, shift, rent, control=1)
{
	print(c(size, shift))
	if (control==1)
	{
		res1 <- ExtraDayMainStrategy(mytab4, c(size, shift), rent, control=1)
		res2 <- ExtraDayMainStrategy(mytab6, c(size, shift), rent, control=1)
		res3 <- ExtraDayMainStrategy(mytab7, c(size, shift), rent, control=1)
	}
	else
	{
		res1 <- ExtraDayMainStrategy(mytab1, c(size, shift), rent, control=1)
		res2 <- ExtraDayMainStrategy(mytab2, c(size, shift), rent, control=1)
		res3 <- ExtraDayMainStrategy(mytab3, c(size, shift), rent, control=1)
	}
	cash <- res1$cash + res2$cash + res3$cash
	transfer <- sum(res1$transfer) + sum(res2$transfer) + sum(res3$transfer)
	deals <- sum(res1$deals) + sum(res2$deals) + sum(res3$deals)
	dealsvec <- c(res1$deals, res2$deals, res3$deals)
	info <- c(res1$cashvec, res2$cashvec, res3$cashvec)
	list(report = c(cash, transfer, deals), deals = dealsvec, info=info)
}
###процедура работы с внедневными стратегиями на тренировочной выборке
ExtraStrat <- function(str, sizes, shifts, rent)
{
print("new")
	for (shift in shifts)
	{
#	for (shift0 in shifts)
#	{
		for (size in sizes)
		{
#		for (size0 in sizes)
#		{
####			if (shift>2*size-1) {next}
#####			if (shift0>2*size-1) {next}
			print(c(size, shift))
			res1 <- ExtraDayMainStrategy(mytab1, c(size, shift, shift, size), rent, 1)
			res2 <- ExtraDayMainStrategy(mytab3, c(size, shift, shift, size), rent, 1)
			res3 <- ExtraDayMainStrategy(mytab6, c(size, shift, shift, size), rent, 1)
			####ressum <- res1$cash + res2$cash + res3$cash
			write.table(rbind(c(),c(ratio, maxval, divisor, res1$cash + res2$cash + res3$cash, sum(res1$transfer) + sum(res2$transfer) + sum(res3$transfer), sum(res1$deals) + sum(res2$deals) + sum(res3$deals))), str, append=TRUE, row.names=FALSE, col.names=FALSE)
		}
#		}
#	}
	}
}

###функция переноса полученных активов на следующий день
Transfer <- function(assets)
{
-75*assets*100*0.07/(365)
}
###функция определения среднего на основе регрессии вектора средних от количества дней до экспирации
###вход: vec1 --- вектор средних, vec2 --- вектор дней до экспирации
###регрессия проводится по формуле: vec1 = a*vec2 + 0 (в конце экспирации ставка 0)
###day --- день, за который необходимо определить среднее
###выход: число, среднее в день day согласно регрессии
IdentifyMean <- function(vec1, vec2, day)
{
	mm <- lm(vec1 ~ vec2 + 0)	
	kef <- as.numeric(mm$coefficients)
###	print(kef)
	day*kef
}
####функция, создающая трубку
GetTube <- function(moment, day, skip, tab, ratio)
{
	tabpast <- tab[((tab$day>=day-skip) & (tab$day<day)),]
	tabcurr <- tab[(tab$day==day),]
	if (moment>=dim(tabcurr)[1]) {print("wrong choice of time moment!!!")}
	else
	{
		if (moment>0)
			{tabpast <- rbind(tabpast, tabcurr[1:moment,])}
		tabcurr <- tabcurr[((moment+1):dim(tabcurr)[1]),]
		vecpast <- (tabpast$bid + tabpast$ask)/2
		veccurr <- (tabcurr$bid + tabcurr$ask)/2
		vec1past <- 1:dim(tabpast)[1]
		vec1curr <- (dim(tabpast)[1] + 1):(dim(tabpast)[1] + dim(tabcurr)[1])
		mm <- lm(vecpast ~ vec1past)
		kefs <- as.numeric(mm$coefficients)
		errors <- as.numeric(mm$residuals)
		q1 <- as.numeric(quantile(errors, ratio))
		q2 <- as.numeric(quantile(errors, 1-ratio))
#		print("inside")
#		print(c(q1,q2))
#		print(kefs)
#		plot(vec1past, vecpast, type="l")
#		abline(kefs, col="red")
#		abline(c(kefs[1]+q1, kefs[2]), col="blue")
#		abline(c(kefs[1]+q2, kefs[2]), col="blue")
#		plot(vec1curr, veccurr, type="l")
#		abline(kefs, col="red")
#		abline(c(kefs[1]+q1, kefs[2]), col="blue")
#		abline(c(kefs[1]+q2, kefs[2]), col="blue")
	}	
list(kefs=kefs,qq=c(q1,q2), end=kefs[1]+kefs[2]*(moment+1))
}
###стратегия, базирующаяся не на одном дне
###вход: tab --- данные за день
###rent --- скольжение (с учетом комиссии)
###mymean --- среднее
###params --- другие параметры модели (а именно пара чисел, размер рамки и сдвиг рамки)
###assets --- число активов, купленных моделью
###needtoclose --- булевская переменная, надо ли закрывать позицию
###выход: список с полями
###$cash --- кумулятивная прибыль
###$transfer --- сумма, которая переходит на следующий день из-за не закрытия позиции 
###$num --- число сделок за день
###$assets --- число активов
ExtraStrategy <- function(tab, rent, mymean, params, assets, needtoclose, control, uplim=1, stratnum=1)
{
	needtoclose <- TRUE
	
	###mymean <- (tab$bid[1] + tab$ask[1])/2

	size <- params[1]; shift <- params[2]; shift0 <- params[3]; size0 <- params[4]
	if (is.na(shift0)) {shift0 <- shift}
	if (is.na(size0)) {size0 <- size}

	num <- 0
#####	if (abs(assets)>1) {print("WTF???")}
	if (assets>0)
	{
		asknum <- assets
		bidnum <- 0		
	}
	else if (assets<0)
	{
		bidnum <- -assets
		asknum <- 0
	}
	else 
	{
		bidnum <- 0
		asknum <- 0
	}
	modshift <- 0
	modshift0 <- 0
#####	uplim <- 1
	cash <- 0

#	print(mymean)
#	print(params)
#	print(c(mymean-size0+modshift0, mymean+size+modshift))

if (stratnum==1)
{
	if (needtoclose) {stoploss <- -100*uplim} ###{stoploss <- -100}
	else {stoploss <- -300*uplim}
	waitparam <- 60
	broken <- FALSE

	bidprev <- min(tab$bid[1], tab$ask[1])
	askprev <- max(tab$ask[1], tab$bid[1])

####	NN <- 50

#####	for (j in (NN+1):dim(tab)[1])
	for (j in (1):dim(tab)[1])
	{

####		sdlocal <- sd((tab$bid[1:NN] + tab$ask[1:NN])/2)
####		size <- max(2.1*sdlocal, 5)
####		shift <- size/2
####		size0 <- size
####		shift0 <- shift

######		print(c(size, shift, shift0, size0))

		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					print("<<<<")
					break
				}
			}
		}
		else {broken <- FALSE}

if (strategycontrol == "FULL")
{
	minushack <- -1000
	plushack <- 1000
}
else if (strategycontrol == "PLUS")
{
	minushack <- -1000
	plushack <- 0
}
else if (strategycontrol == "MINUS")
{
	minushack <- 0
	plushack <- 1000
}
if (assets > minushack)
{
		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidcur>mymean+size+modshift) & (bidprev<mymean+size+modshift)))
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>0)))
			{
				if (control==1)
				{
					print("!!!!")
					pricebid <- max(bidcur, mymean + size + modshift)
				}
				else
				{
					pricebid <- mymean + size + modshift
				}
				vec <- Sell(pricebid - rent, 1)

			if (withlog)
			{
		write.table(rbind(c(),c(nnstrat, size, modshift, 0, -1, pricebid, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
			}

				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				bidnum <- bidnum + 1

#print(mymean)
#print(c(mymean-size0+modshift0, mymean+size+modshift))


			}

		}
}
	if (assets<plushack)
	{
		if ((abs(askcur - mymean + size0 - modshift0)<0.0000001) | ((askcur < mymean - size0 + modshift0) & (askprev>mymean-size0+modshift0)))
		{
			if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<0)))
			{
				if (control==1)
				{
					print("!!!!")
					priceask <- min(askcur, mymean - size0 +  modshift0)
				}
				else
				{
					priceask <- mymean - size0 +  modshift0
				}
#####				vec <- Buy(priceask + rent, 1)
				vec <- SmartBuy(priceask + rent, assets)

			if (withlog)
			{
		write.table(rbind(c(),c(nnstrat, size0, modshift0, 0, 1, priceask, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
			}

				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				asknum <- asknum + 1

#print(mymean)
#print(c(mymean-size0+modshift0, mymean+size+modshift))

			}
		}	
	}
		if ((assets > uplim) | (abs(assets - uplim)<0.0000001))
		{
			modshift0 <- -floor(abs(assets)/uplim)*shift0
			modshift <- - abs(assets)*shift/uplim
		}	
		else if ((-assets > uplim) | (abs(-assets - uplim)<0.0000001))
		{
			modshift <- floor(abs(assets)/uplim)*shift
			modshift0 <- floor(abs(assets)/uplim)*shift0
		}
		else
		{
			modshift <- 0
			modshift0 <- 0
		}

		bidprev <- bidcur
		askprev <- askcur
	}

}
else if (stratnum==2)
{
	####hardcode 
	N <- 360
	stoploss <- -100
	waitparam <- 60

	bidprev <- min(tab$bid[N+2], tab$ask[N+2])
	askprev <- max(tab$ask[N+2], tab$bid[N+2])
	modshift <- 0

	broken <- FALSE
	baddate <- 0	
if (dim(tab)[1] > (N+2))
{
	for (j in (N+2):dim(tab)[1])
	{

		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}


		if (j==(N+2))
			{mymean <- MovingAverage(N, tab, j-1)}
		else
			{
				mymean <- mymean + (-tab$bid[j-N-2]-tab$ask[j-N-2]+tab$bid[j-1]+tab$ask[j-1])/(2*(N+1))
			}

		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidprev < mymean + size + modshift) & (bidcur>mymean+size+modshift)))
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>0)))
			{
				if (control==1)
				{
					pricebid <- max(bidcur, mymean + size + modshift)
				}
				else
				{
					pricebid <- round(mymean + size + modshift)
				}
				vec <- Sell(pricebid - rent, 1)

				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				bidnum <- bidnum + 1

				####asknum <- 0
			}

		}
		if ((abs(askcur - mymean + size - modshift)<0.0000001) | ((askprev > mymean - size + modshift) & (askcur<mymean-size+modshift)))
		{
			if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<0)))
			{
				if (control==1)
				{
					priceask <- min(askcur, mymean - size +  modshift)
				}
				else
				{
					priceask <- round(mymean - size + modshift)
				}
				###vec <- Buy(priceask + rent, 1)
				vec <- SmartBuy(priceask + rent, assets)

				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				asknum <- asknum + 1

				####bidnum <- 0
			}
		}	

		if ((assets > uplim) | (abs(assets - uplim)<0.0000001))
		{
			modshift <- -floor(abs(assets)/uplim)*shift
		}	
		else if ((-assets > uplim) | (abs(-assets - uplim)<0.0000001))
		{
			modshift <- floor(abs(assets)/uplim)*shift
		}
		else
		{
			modshift <- 0
		}


		bidprev <- bidcur
		askprev <- askcur
	}
}	

}
	if (needtoclose)
	{
		pricebid <- bidcur
		priceask <- askcur

		if (withlog)
		{
		if (assets>0)
		{
			write.table(rbind(c(),c(nnstrat, size, modshift, 0, -assets, pricebid, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
		}
		else if (assets<0)
		{
			write.table(rbind(c(),c(nnstrat, size0, modshift0, 0, -assets, priceask, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
		}
		}

		cash <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
		assets <- 0
		transfer <- 0
	}
	else
	{
		transfer <- Transfer(assets)
	}

	list(cash=cash,  transfer=transfer, num=num, assets=assets)
}
###обертка для стратегии, базирующейся не на одном дне
###вход: дата-фрейм с теми же столбцами
###добавляется столбец $expdays --- число дней до экспирации
###params --- вектор параметров для стратегии
###rent --- скольжение (с учетом комиссии)
###выход: список с полями
###$meanvec --- вектор реальных средних по дням
###$expdays --- вектор колчиств дней до экспирации
###$mymeanvec --- вектор реально выставленных средних в начале каждого дня
###$cash --- прибыль модели
###$cashvec --- вектор прибыли модели
###$transfer --- вектор прибыли из-за переносов
###$deals --- вектор из количества сделок
###$currentstate --- вектор текущих состояний
###$cashstate
ExtraDayMainStrategy <- function(tab, params, rent, control)
{
print("in")

	uplim <- 1
	print(c(control, uplim, params))
	days <- unique(tab$day)
	skip <- 1
	print(paste("skip ", skip))
	meanvec <- c()
	expdays <- c()
	mymeanvec <- c()
	cashvec <- c()
	transfer <- c()
	cash <- 0
	assets <- 0
	deals <- c()
	datecut <- 15*60
	dayprev <- tab$expdays[1]
	transfercost <- 0
	allassets <- c()
	currentstate <- c()
	cashstate <- c()
	mymeanprev <- 0

	###отбрасываем последний день
	days <- days[1:(length(days) - 1)]

	for (j in 1:length(days))
	{
		if (j<=skip) 
		{
			day <- days[j]
			tabred <- tab[tab$day == day,]

			###отсеиваем первые и последние 15 минут
			datemax <- max(tabred$date)
			datemin <- min(tabred$date)
			tabred <- tabred[((tabred$date<=datemax-datecut) & (tabred$date>=datemin+datecut)),]
			if (dim(tabred)[1] < 10) {skip<-skip+1; next}

			expdays <- c(expdays, tabred$expdays[1])
			meanvec <- c(meanvec, CalculateMean(tabred, 86400))
			mymeanvec <- c(mymeanvec, 0)
		#####	deals <- c(deals, 0)
			next
		}
		else
		{
			day <- days[j]
			tabred <- tab[tab$day == day,]

			###отсеиваем первые и последние 15 минут
			datemax <- max(tabred$date)
			datemin <- min(tabred$date)
			tabred <- tabred[((tabred$date<=datemax-datecut) & (tabred$date>=datemin+datecut)),]
			if (dim(tabred)[1] < 10) {next}

			if (control==0)
			{
				meanvecred <- meanvec
				expdaysred <- expdays
			}
			else
			{
				#meanvecred <- meanvec[(length(meanvec)-skip+1):(length(meanvec))]
				#expdaysred <- expdays[(length(meanvec)-skip+1):(length(meanvec))]
			}

			if (control==0)
			{
				mymean <- IdentifyMean(meanvecred, expdaysred, tabred$expdays[1])
				if ((mymean > mymeanprev) & (assets<0)) {mymean <- mymeanprev}
				mymeanprev <- mymean
			}
			else
			{
				print("Warning!!!!!")
			#	tabredmod <- tab[((tab$day<day) & (tab$day>=days[j - skip])),]
			#	vec1 <- (tabredmod$bid + tabredmod$ask)/2
			#	vec2 <- 1:dim(tabredmod)[1]
			#	mm <- lm(vec1~vec2)
			#	kefs <- as.numeric(mm$coefficients)
			#	mymean <- kefs[1] + kefs[2]*vec2[dim(tabredmod)[1]]
				
####ratio <- 0.05
				tube <- GetTube(0, day, days[j]-days[j-skip], tab, ratio)
				mymean <- tube$end

				if ((assets<0) & (mymean > mymeanprev)) {mymean <- mymeanprev}
				mymeanprev <- mymean
				###print(mymean)

				val1 <- tube$qq[1]
				val2 <- tube$qq[2]
###maxval <- 5
###divisor <- 1/3
				if (val2 <= maxval) {val2 <- maxval}
				if (val1 >= -maxval) {val1 <- -maxval}
				params <- c(val2, val2*divisor, -val1*divisor, -val1)
		#		params <- c(val2, 0, 0, -val1)
				#print(mymean)
		#		print(params)
				#print(tube$qq)
				#print("****")
			}			

			mymeanvec <- c(mymeanvec, mymean)

			if (j<length(days))
				{res <- ExtraStrategy(tabred, rent, mymean, params, assets, FALSE, control=0, uplim=uplim, stratnum=1)}
			else
				{res <- ExtraStrategy(tabred, rent, mymean, params, assets, TRUE, control=0, uplim=uplim, stratnum=1)}

			if (assets!=0) {print("position is not closed")}

			cash <- cash + transfercost*(dayprev - tabred$expdays[1]) + res$cash

			####проводим проверку текущей ситуации по последней цене за день
			pricebid <- median(pmin(tabred$bid, tabred$ask)) 
			priceask <- median(pmax(tabred$bid, tabred$ask))

			assets <- res$assets

			#print(c(pricebid, priceask))
			#print(c(cash, assets))
			#print("***")

			currentstate <- c(currentstate, cash + Normalize(cash, assets, pricebid-rent, priceask+rent))
			cashstate <- c(cashstate, cash)

			deals <- c(deals, res$num)
			cashvec <- c(cashvec, res$cash)
			transfer <- c(transfer, transfercost*(dayprev - tabred$expdays[1]))

			expdays <- c(expdays, tabred$expdays[1])
			meanvec <- c(meanvec, CalculateMean(tabred, 86400))
			dayprev <- tabred$expdays[1]
			transfercost <- res$transfer
			allassets <- c(allassets, assets)
		}
	}

	list(meanvec=meanvec, expdays=expdays, mymeanvec=mymeanvec, cash=cash, cashvec=cashvec, transfer=transfer, deals=deals, assets=allassets, currentstate=currentstate, cashstate=cashstate)
}

###Эта функция выводит список дней с максимальными скачками среднего
###Исключаются 7 дней с самыми большими скачками
###Выход: матрица со столбцами день, размер максимального скачка
ChooseDays <- function(tab)
{
	days <- unique(tab$day)
	days <- days[days>0]
	res <- cbind(c(), c())
	for (day in days)
	{
		print(day)
		tabred <- tab[tab$day==day,]
		vec <- (tabred$bid + tabred$ask)/2
		tmax <- max(abs(diff(vec)))
		res <- rbind(res, c(day, tmax))	
	}
	res
}

###Эта функция используется для составления отчета
###вход: дата-фрейм с данными,
###параметры модели и ее номер
###выход: список
###$report --- результат работы функции GetReport
###$resodd, $reseven --- результаты работы
MakeReport <- function(tab,size, shift, delay, N, idnum, rent, uplim)
{
	resodd <- MainStrategy(tab, size, shift, delay, idnum, rent, "odd", uplim, N, 0)
	resodd <- as.data.frame(resodd)
	names(resodd) <- c("maxlot", "pl", "assets", "num", "maxcash","sdprev","sdcur","day")
	reseven <- MainStrategy(tab, size, shift, delay, idnum, rent, "even", uplim, N, 0)
	reseven <- as.data.frame(reseven)
	names(reseven) <- c("maxlot", "pl", "assets", "num", "maxcash","sdprev","sdcur","day")
	list(report=GetReport(resodd, reseven, rent), resodd=resodd, reseven=reseven)
}

###Эта функция используется для составления отчета
###вход data.frame с полями $maxlot, $pl, $assets, $num, $maxcash (аналог того, что возвращается стратегиями)
###Выход: список с полями, каждое поле --- это вектор, первая колонка соответствует нечетным дням, вторая четным
###$profs: p&l
###$maxlots: максимальное число открытых позиций
###$profsperlot: (суммарный p&l)/(максимальное число открытых позиций за день)
###$pros --- максимальная просадка
###$sharps: Sharpe ratio
###$sortino: Sortino ratio
###$vec1 --- просадка в процентах для tab1
###$vec2 --- просадка в процентах для tab2
###$worked --- процент дней, в которых была сделана хотя бы 1 сделка
GetReport <- function(tab1, tab2, rent)
{
GO <- 15
	profs <- c(sum(tab1$pl), sum(tab2$pl))
	maxlots <- c(max(tab1$maxlot), max(tab2$maxlot))
	profsperlot <- c( (sum(tab1$pl) /GO)*(365/dim(tab1)[1]), 
		(sum(tab2$pl)/GO)*(365/dim(tab1)[1]))
	pros1 <- GetProsadka(tab1)
	pros2 <- GetProsadka(tab2)
	pros <- c(max(pros1), max(pros2))
	sharps <- c(mean(tab1$pl)/sd(tab1$pl), mean(tab2$pl)/sd(tab2$pl))	
	sortinos <- c(mean(tab1$pl)/sd(tab1$pl[tab1$pl<0]), mean(tab2$pl)/sd(tab2$pl[tab2$pl<0]))
	worked <- c(length(tab1$num[tab1$num>0])/length(tab1$num), length(tab2$num[tab2$num>0])/length(tab2$num))

	list(profs=profs, maxlots=maxlots, profsperlot=profsperlot, pros=pros, sharps=sharps,
		sortinos=sortinos, vec1=pros1, vec2 = pros2, worked=worked)
}

###Эта функция выводит информацию о просадке
###вход дата-фрейм со столбцом $pl
GetProsadka <- function(tab)
{
	cumsumma <- cumsum(tab$pl)
	maxcumsum <- -Inf
	prosadka <- c()
	for (j in 1:dim(tab)[1])
	{
		maxcumsum <- max(cumsumma[1:j])
		prosadka <- c(prosadka, (maxcumsum - cumsumma[j])/abs((maxcumsum + 0.00000000001)))
	}
	prosadka
}

###Эта функция рисует графики
###Вход: day --- номер дня, N --- период скользящего среднего
###Рисуется график bid, ask (красным) и скользящего среднего (синим)
###на данный момент скользящее среднее закомменчено
DrawGraph <- function(tab, day, N)
{
	tabred <- tab[tab$day==day,]	

	###print(dim(tabred))

	vec <- MovingAverageCalc(N, tabred)
	#####tabred <- tabred[(N+1):dim(tabred)[1],]

        ###print(summary(tabred$date))
        ###print(summary(pmin(tabred$bid, tabred$ask)))

	plot(tabred$date, pmin(tabred$bid, tabred$ask), type="l", ylim = c(2.9, 3.3))
	lines(tabred$date, pmax(tabred$bid, tabred$ask), col="red")	
	lines(tabred$date[(N+1):dim(tabred)[1]], vec+0.08, col="blue")
	lines(tabred$date[(N+1):dim(tabred)[1]], vec-0.08, col="green")
lines(tabred$date[(N+1):dim(tabred)[1]], vec, col="blue")
}

###Эта функция запускает стратегии, связанные со скользящим средним (Стратегию 7).
###Вход: sizes --- вектор размеров рамки (рамка имеет размер от mean-size до mean+size)
###shifts --- вектор сдвигов рамки
###delays --- вектор задержек (целое число)
###задержка k означает, что если у нас m единиц актива и m>k, то
###мы осуществляем сдвиг всей конструкции на k*shifts*floor(m/k) пунктов вверх
###тоже самое и с продажей
###Ns --- вектор периодов, по которому используется скользящее среднее
###str --- имя файла, куда записываются результаты
###mode: "odd" или "even" --- нечетные или четные дни надо рассматривать?
###Функция записывает результаты работы стратегии в файл
###Эти результаты потом можно просмотреть и выбрать наилучшие
###Структура информации, записанной в файл:
###названия столбцов (для удобства тех, кто любит читать txt-файл блокнотом) отсутствуют
### size, shift, delay, N aka параметры модели, первые 4 столбца
###средняя прибыль модели, среднее число сделок модели, среднее стандартное отклонение --- следующие 3 столбца
###медиана прибыли модели, Sharpe ratio --- последние 2 столбца
StrategiesMA <- function(tab, sizes, shifts, delays, Ns, uplims, str, mode="odd", rent)
{
for (uplim in uplims)
{
	for (size in sizes)
	{
#####if (size<0.12) {next}
#for (size0 in sizes)
#{
	for (shift in shifts)
	{
	if (shift>2*size) {next}
#for (shift0 in shifts)
#{
#if (shift0>2*size0) {next}
	for (delay in delays)
	{
	for (N in Ns)
	{
print("starting")
print(c(uplim, size))
		if ((shift==0) & (delay>1)) {break}
		res <- MainStrategy(tab, size=size, shift=shift, delay=delay, rent=rent, mode=mode, idnum=9, uplim=uplim, N=N, control=0)
		line <- rbind(c(),c(uplim, size, shift, delay, N, mean(res[,2]), mean(res[,4]), dim(res[res[,4]>0,])[1]/dim(res)[1], median(res[,2]), mean(res[,2])/sd(res[,2])))
		write.table(line,str,append=TRUE,row.names=FALSE,col.names=FALSE)
	}
	}
#}
	}
#}
	}
}
}

###Not run
StrategiesVAR <- function(tab, shifts, Ns, str, mode="odd")
{
	for (shift in shifts)
	{
	for (N in Ns)
	{
		res <- MainStrategy(tab, size=NA, shift=shift, delay=1, rent=1.6, mode=mode, idnum=8, uplim=100, N=N)
		line <- rbind(c(),c(shift, N, sum(res[,2]), mean(res[,2]), mean(res[,4]), mean(res[,6]), mean(res[,5]), median(res[,2]), mean(res[,1]), median(res[,1])))
		write.table(line,str,append=TRUE,row.names=FALSE,col.names=FALSE)
	}
	}

}


###Эта функция записывает результаты различных стратегий, работающих без скользящего среднего в файл для последующего анализа
###Список входных параметров --- см. StrategiesMA
###Структура итогового файла: первые 3 столбца параметры модели
###следующие 5 столбцов: 
###средняя прибыль, среднее количество сделок, среднее стандартное отклонение, медиана прибыли, коэффициент Шарпа 
StrategiesFull <- function(tab, sizes=1:15, shifts=0:10, delays=c(1, 10, 30, 50, 101), str="res.txt", mode="odd")
{
	for (size in sizes)
	{
		for (shift in shifts)
		{
			if (shift>2*size-1) {next}
			for (delay in delays)
			{
				if ((shift==0) & (delay>1)) {break}
				res <- MainStrategy(tab, size=size, shift=shift, delay=delay, rent=1.6, mode=mode, idnum=0, uplim=100, control=0)
				line <- rbind(c(),c(size, shift, delay, mean(res[,2]), mean(res[,4]), mean(res[,6]), median(res[,2]), mean(res[,2])/sd(res[,2])))
				write.table(line,str,append=TRUE,row.names=FALSE,col.names=FALSE)
			}
		}
	}
#	uplimvec <- c(1, 2, 5, 10, 15, 20, 25, 30)
#	for (size in sizes)
#	{
#		for (rent in rents)
#		{
#			for (uplim in uplimvec)
#			{
#				res <- MainStrategy(tab, size=size, shift=NA, delay=NA, rent=rent, mode=mode, idnum=1, uplim=uplim)
#				line <- rbind(c(),c(size, rent, uplim, sum(res[,2]), mean(res[,2]), mean(res[,4]), mean(res[,6]), mean(res[,5]), median(res[,2]), mean(res[,2]*(res[,4]))))	
#				write.table(line,str,append=TRUE,row.names=FALSE,col.names=FALSE)
#			}
#		}
#	}

}

###Эта функция используется для получения информации о конкретной стратегии, она как раз и вызывается
###StrategiesMA, StrategiesVAR или StrategiesFull
###вход:
###idnum --- идентификатор конкретной модели
### (0 --- без скользящего среднего, 7 --- со скользящим средним)
###rent, uplim --- эти параметры не играют никакой роли, потому что в конкретных стратегиях
###используется hardcode на данный момент
###выход: матрица
###первые столбцы --- вся информация, возвращаемая конкретной стратегией
###потом стандартное отклонение предыдущего дня, стандартное отклонение текущего дня, номер дня
###
###еще tab должна содержать $periodnum --- номер конкретного периода
MainStrategy <- function(tabb, size, shift=NA, delay=NA, idnum=1, rent=0, mode="odd", uplim=25, N, control, size0=size, shift0=shift)
{ 
	print("NEW")
	resdata <- data.frame(v1=c(), v2=c(), v3=c(), v4=c(), v5=c(), v6=c())
for (periodnum in unique(tabb$periodnum))
{
tab <- tabb[tabb$periodnum == periodnum,]

	print(paste(mode,c(rent, uplim, control)))
	days <- unique(tab$day)
tabdayprev <- c()
	if (mode=="full")
	{
		for (day in days)
		{
			#if (day>days[1])
			#{
			#	tabday <- tabday[(dim(tabday)[1]-N-1):(dim(tabday)[1]),]
			#	tabday <- rbind(tab[tab$day == day,], tabday)				
			#}
			#else
			#{
			#	tabday <- tab[tab$day == day,]
			#}

			tabday <- tab[tab$day == day,]

			datecut <- 15*60
			datemin <- tabday$date[1]
			datemax <- tabday$date[length(tabday$date)]
			tabday <- tabday[(tabday$date>datemin + datecut) & (tabday$date<datemax - datecut),]
			
			####if (dim(tabday)[1]<400) {next}
			resdata <- rbind(resdata, c(AnalyzeDay(tab=tabday, tabprev=tabday, size=size, shift=shift, delay=delay, idnum=idnum, rent=rent, uplim=uplim, N=N, control=control, size0, shift0),day))	
		}
	}
	else if (mode=="odd")
		{
		####days <- days[2:length(days)]
		for (day in days)
		{
			tabday <- tab[tab$day == day,]

			datecut <- 15*60
			datemin <- tabday$date[1]
			datemax <- tabday$date[length(tabday$date)]
			tabday <- tabday[(tabday$date>datemin + datecut) & (tabday$date<datemax - datecut),]
			
			if (dim(tabday)[1]<400) {next}

			if ((day %% 2) == 1)
			{
				if (dim(tabday)[1]>=400)
					{tabdayprev <- tabday}
			}
			else
			{
#			print("dim!!")
#			print(dim(tabdayprev))
#			print(dim(tabday))
				resdata <- rbind(resdata, c(AnalyzeDay(tab=tabday, tabprev=tabdayprev, size=size, shift=shift, delay=delay, idnum=idnum, rent=rent, uplim=uplim, N=N, control=control, size0, shift0),day))
				tabdayprev <- tabday
#####print(summary(resdata))
#####			break
			}
		}
	}
	else if (mode=="even")	
	{
		for (day in days)
		{
			tabday <- tab[tab$day == day,]

			datecut <- 15*60
			datemin <- tabday$date[1]
			datemax <- tabday$date[length(tabday$date)]
			tabday <- tabday[(tabday$date>datemin + datecut) & (tabday$date<datemax - datecut),]

			if (dim(tabday)[1]<400) {next}

			if ((day %% 2) == 0)
			{
				if (dim(tabday)[1]>=400)
					{tabdayprev <- tabday}
			}
			else
			{
				resdata <- rbind(resdata, c(AnalyzeDay(tab=tabday, tabprev=tabdayprev, size=size, shift=shift, delay=delay, idnum=idnum, rent=rent, uplim=uplim, N=N, control=control, size0, shift0),day))
				tabdayprev <- tabday
			}
		}

	}
}
resdata
}

###Эта функция анализирует конкретный день
###Она вызывается MainStrategy
###вход: tabprev --- таблица, по которой следует оценивать среднее положение рамки в начале дня
###в настоящее время не используется, рамка устанавливается вокруг текущей позиции
###используются только стратегии 0 и 7
###возврат: вектор
###информация, возвращаемая кокретной стратегией
###стандартное отклонение предыдущего дня, стандартное отклонение текущего дня
AnalyzeDay <- function(tab, tabprev, size, shift=NA, delay=NA, idnum=1, rent=0, uplim=25, N=0, control, size0, shift0)
{
if (idnum==0)
{
	mymean <- (tab$bid[1] + tab$ask[1])/2
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy0(tab, size, shift, delay, mymean, rent, uplim=uplim), sdprev, sdcur)
}
else if (idnum==7)
{
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy7(tab, size, shift, delay, N, rent, uplim, control=control), sdprev, sdcur)
}
else if (idnum==77)
{
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy77(tab, size, shift, delay, N, rent, uplim, control=control), sdprev, sdcur)
}
else if (idnum==9)
{
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy9(tab, size, size0, shift, shift0, delay, N, rent, uplim), sdprev, sdcur)
}
else if (idnum==99)
{
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy9mod(tab, size, size0, shift, shift0, delay, N, rent, uplim), sdprev, sdcur)
}
else if (idnum==8)
{
	mymean <- (tab$bid[1] + tab$ask[1])/2
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy8(tab=tab, shift=shift, delay=delay, mymean=mymean, N=N), sdprev, sdcur)
}
else if (idnum==1)
{
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy1(tab, size, shift, rent, uplim=uplim), sdprev, sdcur)
}
else if (idnum==2)
{
###	mymean <- CalculateMean(tabprev)
	mymean <- (tab$bid[1] + tab$ask[1])/2
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy2(tab, size, shift, delay, mymean, rent, uplim=uplim), sdprev, sdcur)
}
else if (idnum==2.5)
{
	mymean <- (tab$bid[1] + tab$ask[1])/2
###	mymean <- CalculateMean(tabprev)
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy2(tab, max(ceiling(sdprev),1), 1, delay, mymean, rent), sdprev, sdcur)
}
else if (idnum==3)
{
	mymean <- (tab$bid[1] + tab$ask[1])/2
####	mymean <- CalculateMean(tabprev)
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy3(tab, size, shift, delay, mymean, rent), sdprev, sdcur)
}
else if (idnum==3.5)
{
	mymean <- (tab$bid[1] + tab$ask[1])/2
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy3(tab, size, shift, delay, mymean, rent), sdprev, sdcur)
}
else if (idnum==4)
{
	mymean <- CalculateMean(tabprev)
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy4(tab, size, shift, mymean, rent), sdprev, sdcur)
}
else if (idnum==4.5)
{
	mymean <- (tab$bid[1] + tab$ask[1])/2
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy4(tab, size, shift, mymean, rent), sdprev, sdcur)
}

resdata
}

###Эта функция вычисляет среднее за период
###может ипользоваться для опредления положения рамки в начале
CalculateMean <- function(tab, period = 6*60*60)
{
	datemax <- tab$date[dim(tab)[1]]
	avdata <- tab[tab$date>datemax - period,]
	num <- (mean(avdata$bid) + mean(avdata$ask))/2	
	round(num)
}

###функция продажи num-единиц актива по цене price
Sell <- function(price, num)
{
	cash <- price*num
	assets <- -num
	c(cash, assets)
}
###функция продажи по цене price с учетом имеющихся активов
SmartSell <- function(price, assets)
{
	if (assets>0) {vec <- Sell(price, assets)}
	else {vec <- Sell(price, 1)}
	vec
}
###функция покупки num-единиц актива по цене price
Buy <- function(price, num)
{
	cash <- -price*num
	assets <- num	
	c(cash, assets)
}
###функция покупки по цене price с учетом имеющихся активов
SmartBuy <- function(price, assets)
{
	if (assets<0) {vec <- Buy(price,-assets)}
	else {vec <- Buy(price, 1)}
	vec
}
###функция рассчетов в конце дня
Normalize <- function(cash, assets, pricebid, priceask)
{
	if (assets<0)
	{
		vec <- Buy(priceask, -assets)
	}
	else if (assets>0)
	{
		vec <- Sell(pricebid, assets)
	}
	else
	{
		vec <- c(cash, assets)
	}
	vec[1]
}
###функция вычисления скользящего среднего
###за период N в момент j
###используется фактически лишь при j = N+1
MovingAverage <-  function(N, tab, j)
{
	if (j<=N) 
	{
		vecbid <- tab$bid[1:j]
		vecbid <- c(rep(tab$bid[1],N+1-j),vecbid)
		vecask <- tab$ask[1:j]
		vecask <- c(rep(tab$ask[1],N+1-j),vecask)
	}
	else
	{
		vecbid <- tab$bid[(j-N):j]
		vecask <- tab$ask[(j-N):j]
	}
	(sum(vecbid) + sum(vecask))/(2*(N+1))
}
###функция вычисления скользящего среднего за период N
###выход: матрица
###момент времени, значение скользящего среднего в этот момент времени
MovingAverageCalc <- function(N, tab)
{
	vec <- c()
	for (j in (N+1):dim(tab)[1])
	{
		vec <- c(vec, MovingAverage(N, tab, j))
	}
	vec
}
###Not run
MovingVar <- function(N, tab, j)
{
###hardcode
kef <- 0.7321
kef2 <- 1.9164
#kef <- 1.9169
#kef2 <- 0.0438
#####kef=0.6435, 1.6654
	if (j<=N) {res <- 2}
	else 
	{
		vec <- tab$bid[(j-N):j]
		res <- kef*sd(vec) + kef2
		
		if (res<2) {res <- 2}
		if (res>10) {res <- 10}
	}
res
}
###Not run
Strategy8 <- function(tab, shift, delay, mymean, N)
{
	###HARDCODE
	rent <- 3.6
	uplim = 100

	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0
	bidprev <- min(tab$bid[1], tab$ask[1])
	askprev <- max(tab$ask[1], tab$bid[1])
	modshift <- 0

	sizes <- c()

	for (j in (N+1):dim(tab)[1])
	{
		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

		size <- MovingVar(N, tab, j)

#######		shift <- 2*size - 5

		sizes <- c(sizes, size)
		
		if ((bidcur == mymean + size + modshift) | ((bidprev < mymean + size + modshift) & (bidcur>mymean+size+modshift)))
		{
			if (bidnum<uplim)
			{
				pricebid <- max(bidcur, mymean + size + modshift)
				vec <- Sell(pricebid - rent, 1)
				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				bidnum <- bidnum + 1
				asknum <- 0
			}

		}
		if ((askcur == mymean - size + modshift) | ((askprev > mymean - size + modshift) & (askcur<mymean-size+modshift)))
		{
			if (asknum<uplim)
			{
				priceask <- min(askcur, mymean - size +  modshift)
				vec <- Buy(priceask + rent, 1)
				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				asknum <- asknum + 1
				bidnum <- 0
			}
		}	

		if (assets >= delay)
		{
			modshift <- -floor(abs(assets)/delay)*shift
		}	
		else if (-assets >= delay)
		{
			modshift <- floor(abs(assets)/delay)*shift
		}
		else
		{
			modshift <- 0
		}
		

		bidprev <- bidcur
		askprev <- askcur
	}
	pricebid <- min(tab$bid[dim(tab)[1]], tab$ask[dim(tab)[1]])
	priceask <- max(tab$bid[dim(tab)[1]], tab$ask[dim(tab)[1]])
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	c(mean(sizes), cashR, assets, num)
}
###Рамка, построенная вокруг скользащего среднего
###Выход: вектор
###максимальная открытая позиция, p&l
######количество активов (со знаком) до закрытия 
###количество сделок без учета закрытия
###максимальная задействованная сумма (без учета ГО)
Strategy7 <- function(tab, size, shift, delay, N, rent, uplim, control)
{
####	print(rent)
	###HARDCODE
####	rent <- 3.6
####	uplim = 1
####print(uplim)
	stoploss <- -100
####	stoploss <- -Inf
	waitparam <- 60
######print(waitparam)

	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0

	maxlot <- 0
	maxcash <- 0
	bidprev <- min(tab$bid[N+2], tab$ask[N+2])
	askprev <- max(tab$ask[N+2], tab$bid[N+2])
	modshift <- 0

	broken <- FALSE
	baddate <- 0	

	for (j in (N+2):dim(tab)[1])
	{

		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

####		print(c(bidnum, asknum))

		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}


		if (j==(N+2))
			{mymean <- MovingAverage(N, tab, j-1)}
		else
			{
				mymean <- mymean + (-tab$bid[j-N-2]-tab$ask[j-N-2]+tab$bid[j-1]+tab$ask[j-1])/(2*(N+1))
			}

#		if ((j-N-1) %in% c(263, 285, 286, 301))
#			{print(paste(j-N-1, bidcur, askcur, mymean-size+modshift, mymean+size+modshift))}

		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidprev < mymean + size + modshift) & (bidcur>mymean+size+modshift)))
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>=0)))
			{
#####print(paste("sell",j-N-1, bidcur, askcur, mymean-size+modshift, mymean+size+modshift))
				if (control==1)
				{
					pricebid <- max(bidcur, mymean + size + modshift)
				}
				else
				{
					pricebid <- (mymean + size + modshift)
				}
				vec <- Sell(pricebid - rent, 1)

			####write.table(rbind(c(),c(8, size, shift, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)


				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				bidnum <- bidnum + 1

				####asknum <- 0
			}

		}
		if ((abs(askcur - mymean + size - modshift)<0.0000001) | ((askprev > mymean - size + modshift) & (askcur<mymean-size+modshift)))
		{
			if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<=0)))
			{
#print(paste("buy",j-N-1, bidcur, askcur, mymean-size+modshift, mymean+size+modshift))

				if (control==1)
				{
					priceask <- min(askcur, mymean - size +  modshift)
				}
				else
				{
					priceask <- (mymean - size + modshift)
				}
				vec <- Buy(priceask + rent, 1)

		####write.table(rbind(c(),c(8, size, shift, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)


				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				asknum <- asknum + 1

				####bidnum <- 0
			}
		}	

		if ((assets > delay) | (abs(assets - delay)<0.0000001))
		{
			modshift <- -floor(abs(assets)/delay)*shift
		}	
		else if ((-assets > delay) | (abs(-assets - delay)<0.0000001))
		{
			modshift <- floor(abs(assets)/delay)*shift
		}
		else
		{
			modshift <- 0
		}

		maxlot <- max(maxlot,abs(assets))
		maxcash <- min(maxcash, cash)

		bidprev <- bidcur
		askprev <- askcur
	}	

	if (broken) {index <- j} else {index <- dim(tab)[1]}
	if (index!=dim(tab)[1]) {print(c(tab$day[j],j)); print(">>>>")}

	pricebid <- min(tab$bid[index], tab$ask[index])
	priceask <- max(tab$bid[index], tab$ask[index])
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	maxcash <- -maxcash
if (assets<0)
{
####write.table(rbind(c(),c(8, size, shift, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
}
else if (assets>0)
{
####write.table(rbind(c(),c(8, size, shift, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
}
	c(maxlot, cashR, assets, num, maxcash)
####	c(maxlot, cash, assets, num, maxcash)
}

###Рамка, построенная вокруг скользящего среднего
###за полчаса до конца смотрим на инфу из метода интервалов
###Выход: вектор
###максимальная открытая позиция, p&l
######количество активов (со знаком) до закрытия 
###количество сделок без учета закрытия
###максимальная задействованная сумма (без учета ГО)
Strategy77 <- function(tab, size, shift, delay, N, rent, uplim, control)
{
###предположения о параметре control:
###1) значение до конца 2) мода

	stoploss <- -100
####	stoploss <- -Inf
	waitparam <- 60

	delta <- 60
	durtoend <- control[1]
	if (is.na(durtoend)) {durtoend <- delta}
	moda <- control[2]
	if (is.na(moda)) {moda <- 0}

	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0

	maxlot <- 0
	maxcash <- 0
	bidprev <- min(tab$bid[N+2], tab$ask[N+2])
	askprev <- max(tab$ask[N+2], tab$bid[N+2])
	modshift <- 0

	broken <- FALSE
	baddate <- 0
	lastdealval <- NA

	timeclose <- dim(tab)[1]	
	gap <- 5

	for (j in (N+2):dim(tab)[1])
	{
		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

		if (timeclose>dim(tab)[1]) {timeclose <- dim(tab)[1]}
		if (j>=timeclose) {break}

####		print(c(bidnum, asknum))

		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}


		if (j==(N+2))
			{mymean <- MovingAverage(N, tab, j-1)}
		else
			{
				mymean <- mymean + (-tab$bid[j-N-2]-tab$ask[j-N-2]+tab$bid[j-1]+tab$ask[j-1])/(2*(N+1))
			}

#		if ((j-N-1) %in% c(263, 285, 286, 301))
#			{print(paste(j-N-1, bidcur, askcur, mymean-size+modshift, mymean+size+modshift))}

		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidprev < mymean + size + modshift) & (bidcur>mymean+size+modshift)))
		{
			if ((j<timeclose-gap) | ((j>=timeclose-gap) & assets>0))
			{
				if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>0)))
				{
#				print(paste("sell",j, bidcur, askcur, mymean-size+modshift, mymean+size+modshift))
					pricebid <- (mymean + size + modshift)
					vec <- Sell(pricebid - rent, 1)

					lastdealval <- pricebid

					####write.table(rbind(c(),c(8, size, shift, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)

					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					bidnum <- bidnum + 1

					####asknum <- 0
				} else {}
			} else {}
		} else {}
		if ((abs(askcur - mymean + size - modshift)<0.0000001) | ((askprev > mymean - size + modshift) & (askcur<mymean-size+modshift)))
		{
			if ((j<timeclose-gap) | ((j>=timeclose-gap) & (assets<0)))
			{
				if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<0)))
				{
#					print(paste("buy",j, bidcur, askcur, mymean-size+modshift, mymean+size+modshift))

					priceask <- (mymean - size + modshift)
					vec <- Buy(priceask + rent, 1)

					lastdealval <- priceask

					####write.table(rbind(c(),c(8, size, shift, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)

					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					asknum <- asknum + 1

					####bidnum <- 0
				}
			}
		}	

		if ((assets > delay) | (abs(assets - delay)<0.0000001))
		{
			modshift <- -floor(abs(assets)/delay)*shift
		}	
		else if ((-assets > delay) | (abs(-assets - delay)<0.0000001))
		{
			modshift <- floor(abs(assets)/delay)*shift
		}
		else
		{
			modshift <- 0
		}

		maxlot <- max(maxlot,abs(assets))
		maxcash <- min(maxcash, cash)

#######################################

		if (j == dim(tab)[1] - durtoend)
		{
			hres <- AutomaticClosure(tab, TAB60, TABINFO60, bidcur, assets, delta, 5,  j, cash, durtoend/delta)
			if (moda==2) {timeclose <- hres$J}
				else if (moda==1) {timeclose <- j}
				else if (moda==0) {timeclose <- dim(tab)[1]}
			broken <- TRUE
			if (broken) {j <- timeclose}
			print(paste("timeclose", timeclose, j, moda, dim(tab)[1]))
		}
		if (broken) {break}

#######################################

		bidprev <- bidcur
		askprev <- askcur
	}	

	if (broken) {index <- j} else {index <- timeclose}
	print(paste("index", index, dim(tab)[1], tab$day[1]))
	if (index!=dim(tab)[1]) {print(c(tab$day[j],j)); print(">>>>")}

	pricebid <- min(tab$bid[index], tab$ask[index])
	priceask <- max(tab$bid[index], tab$ask[index])

#	print(paste("pricelast", tab$bid[dim(tab)[1]]))

#	print("end")
#	print(pricebid)
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	maxcash <- -maxcash
if (assets<0)
{
####write.table(rbind(c(),c(8, size, shift, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
}
else if (assets>0)
{
####write.table(rbind(c(),c(8, size, shift, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
}
	c(maxlot, cashR, assets, num, maxcash)
}
###процедура пытается решить закрывать ли позицию
###глобальные переменные:
###centers1, centers2, centers3
AutomaticClosure <- function(tab, globaltab, globaltabinfo, pricecur, assets, delta, step, j, cash, par)
{
	broken <- TRUE
	J <- 0
	if (assets!=0)
	{
	#print(paste("day", tab$day[1]))

		tabred <- tab[((j-delta):j),]
		par1 <- sd(tabred$bid)
		tempvec <- 1:(delta+1)
		temp <- lm(tabred$bid~tempvec)
		par2 <- as.numeric(temp$coefficients[2])
		par3 <- as.numeric(temp$coefficients[1])
		par4 <- pricecur - par3 - par2*(1+delta)

		#print(paste("position", par4))

		#temp <- unique(globaltabinfo$qsdstart)
		#temp <- temp[order(temp)]
		#q1 <- max(temp[temp<=par1])
		#temp <- unique(globaltabinfo$qsdend)
		#temp <- temp[order(temp)]
		#q11 <- min(temp[temp>par1])
		#temp <- unique(globaltabinfo$qtrendstart)
		#temp <- temp[order(temp)]
		#q2 <- max(temp[temp<=par2])
		#temp <- unique(globaltabinfo$qtrendend)
		#temp <- temp[order(temp)]
		#q22 <- min(temp[temp>par2])
		#temp <- unique(globaltabinfo$qshiftstart)
		#temp <- temp[order(temp)]
		#q3 <- max(temp[temp<=par3])
		#temp <- unique(globaltabinfo$qshiftend)
		#temp <- temp[order(temp)]
		#q33 <- min(temp[temp>par3])

		#print(c(q1, q2, q3))
###		group <- globaltabinfo$groupid[((globaltabinfo$qsdstart == q1) & (globaltabinfo$qtrendstart == q2) & (globaltabinfo$qposstart == q3))]
#		group <- globaltabinfo$groupid[((globaltabinfo$qsdstart == q1) & (globaltabinfo$qtrendstart == q2) & (globaltabinfo$qshiftstart == q3))]
####						& (globaltabinfo$qsdend < q11) & (globaltabinfo$qtrendend < q22) & )]
		#group <- globaltabinfo$groupid[((globaltabinfo$qsdstart == q1) & (globaltabinfo$qtrendstart == q2))]

		num1 <- IdentifyGroupMini(par1, centers1)
		num2 <- IdentifyGroupMini(par2, centers2)
		num3 <- IdentifyGroupMini(par4, centers3)
		group <- num3*100 + num2*10 + num1		 

		vals <- cbind(c(), c())
		globaltabred <- globaltab[globaltab$group == group,]

#####		globaltabred <- globaltab

		print(c(7 + round((par)*12), round((par)*12)))

		for (k in 8:(7 + round((par)*12)))
		{	
			###vals <- cbind(vals, globaltabred[,k])
			vals <- cbind(vals,pricecur+globaltabred[,k] - globaltabred$valend)
			###rel2 <- globaltabred[,k] - globaltabred$trend*(1 + (k-7)*step + delta) - globaltabred$shift*(1 + (k-7)*step + delta)
			###rel1 <- globaltabred$valend - globaltabred$trend*(1 + (k-7)*step + delta) - globaltabred$shift*(1 + (k-7)*step + delta) 
			###vals <- cbind(vals, par2*(1+(k-7)*step+delta) + par3 + par4 + rel2 - rel1)
		}

		#print(summary(vals))

		expects <- c()
		for (k in 1:(round((par)*12)))
		{
			expect <- 0
			for (val in vals[,k])
			{
				expect <- expect + (cash + Normalize(cash, assets, val-rent, val+rent))/dim(vals)[1]
			}
			expects <- c(expects, expect)
		}		

		expectcur <- (cash + Normalize(cash, assets, pricecur-rent, pricecur+rent))
		expects <- c(expectcur, expects)
		expects <- rbind(c(0,1:(round(par*12))), expects)
		expects <- expects[,order(expects[2,], decreasing=TRUE)]
		J <- expects[1,1]
		expect <- expects[2,1]

		#print(paste("j", j))
		#print(paste("cash", cash))
		print(paste("assets", assets))
		#print(paste("pricecur", pricecur))
		print(paste("group", group))	
		#print(paste("comparing", expectcur, expect))
		print(paste("result is", c(J, j+J*step)))
		print(expects)
		print(paste("chosen", tab$bid[j + J*step]))
		###if (J<12) {broken <- TRUE}
		broken <- TRUE
	}
list(broken=broken, J = j + J*step)
}
###функция проверки на stoploss
###выход: TRUE --- stoploss, FALSE --- всё в порядке
###рамка, построенная без учета скользящего среднего
###вход: rent, uplim --- используется хардкод
###mymean --- начальное положение центра рамки
###выход:
###вектор
###максимальная открытая позиция, прибыль,
###количество активов (со знаком) до закрытия 
###количество сделок без учета закрытия
###максимальная задействованная сумма денег (без учета ГО)
Strategy0 <- function(tab, size, shift, delay, mymean, rent, uplim)
{
	###HARDCODE
####	rent <- 3.6
	print(rent)
	uplim = 100
	stoploss <- -300
	waitparam <- 3*60

	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0
	maxlot <- 0
	maxcash <- 0
	bidprev <- min(tab$bid[1], tab$ask[1])
	askprev <- max(tab$ask[1], tab$bid[1])
	modshift <- 0

	broken <- FALSE
	baddate <- 0

	for (j in 1:dim(tab)[1])
	{

		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}



		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidprev < mymean + size + modshift) & (bidcur>mymean+size+modshift)))
		{
			if ((bidnum<uplim) | ((assets>=0) & (broken==TRUE)))
			{
				pricebid <- max(bidcur, mymean + size + modshift)
				vec <- Sell(pricebid - rent, 1)
				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				bidnum <- bidnum + 1
				asknum <- 0
			}

		}
		if ((abs(askcur - mymean + size - modshift)<0.000001) | ((askprev > mymean - size + modshift) & (askcur<mymean-size+modshift)))
		{
			if ((asknum<uplim) | ((assets<=0) & (broken==TRUE)))
			{
				priceask <- min(askcur, mymean - size +  modshift)
				vec <- Buy(priceask + rent, 1)
				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				asknum <- asknum + 1
				bidnum <- 0
			}
		}	

		if ((assets > delay) | (abs(assets-delay)<0.000001))
		{
			modshift <- -floor(abs(assets)/delay)*shift
		}	
		else if ((-assets > delay) | (abs(-assets-delay)<0.000001))
		{
			modshift <- floor(abs(assets)/delay)*shift
		}
		else
		{
			modshift <- 0
		}
		
		maxlot <- max(maxlot,abs(assets))
		maxcash <- min(maxcash, cash)

		bidprev <- bidcur
		askprev <- askcur
	}	
	if (broken) {index <- j} else {index <- dim(tab)[1]}

	if (index!=dim(tab)[1]) {print(c(tab$day[j],j)); print(">>>>")}

	pricebid <- min(tab$bid[index], tab$ask[index])
	priceask <- max(tab$bid[index], tab$ask[index])

#	if (broken==TRUE)
#	{
#		print("!!!")
#		print(tab$day[j])
#		print((tab$bid[1]+tab$ask[1])/2)
#		print(c(tab$bid[j],tab$ask[j]))
#		print(assets)
#	}


	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
#	if (broken) {print(cashR)}

	maxcash <- - maxcash
	c(maxlot, cashR, assets, num, maxcash)
}
Strategy1 <- function(tab, size, shift, rent, uplim)
{
	stoploss <- -300
	waitparam <- 60
	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0

	maxlot <- 0
	maxcash <- 0
	bidprev <- min(tab$bid[1], tab$ask[1])
	askprev <- max(tab$ask[1], tab$bid[1])
	modshift1 <- 0
	modshift2 <- 0

	broken <- FALSE
	baddate <- 0	

	eps <- 0.000001

	size1 <- size; size2 <- size; shift1 <- shift; shift2 <- shift

	debugtab <- c()

	for (j in 1:dim(tab)[1])
	{
		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 
		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{
				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}

mymean <- (tab$bid[1] + tab$ask[1])/2

		if (bidcur > mymean + size1 + modshift1 - eps)
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>0)))
			{
				possibpos <- ceiling((bidcur - (mymean + size1 + modshift1 - eps))/shift1)
				if (possibpos > max(uplim + assets,0)) {possibpos <- max(uplim + assets,0)}
				while (possibpos > 0)
				{
					pricebid <- mymean + size1 + modshift1
					vec <- Sell(pricebid - rent, 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					bidnum <- bidnum + 1

					modshift1 <- modshift1 + shift1
					modshift2 <- modshift2 + shift2
					possibpos <- possibpos - 1
					break
				}
			}
		}
		else if (askcur < mymean - size2 + modshift2 + eps)
		{
			if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<0)))
			{
				possibpos <- ceiling((mymean - size2 + modshift2 + eps - askcur)/shift2)

				if (possibpos > max(uplim - assets,0)) {possibpos <- max(uplim - assets,0)}
				while (possibpos > 0)
				{
					priceask <- mymean - size2 + modshift2
					vec <- Buy(priceask + rent, 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					asknum <- asknum + 1

					modshift1 <- modshift1 - shift1
					modshift2 <- modshift2 - shift2
					possibpos <- possibpos - 1 
					break
				}
			}
		}

		maxlot <- max(maxlot,abs(assets))
		maxcash <- min(maxcash, cash)

		bidprev <- bidcur
		askprev <- askcur
	}
	if (broken) {index <- j} else {index <- dim(tab)[1]}
	if (index!=dim(tab)[1]) {print(c(tab$day[j],j)); print(">>>>")}

	pricebid <- min(tab$bid[index], tab$ask[index])
	priceask <- max(tab$bid[index], tab$ask[index])
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	maxcash <- -maxcash

	c(maxlot, cashR, assets, num, maxcash)
}
###Not run
Strategy3 <- function(tab, size, shift, delay, mymean, rent=0)
{
print("strategy3")
	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0
	upshift <- shift
	downshift <- shift
	bidtime <- tab$date[1] - 1
	asktime <- tab$date[1] - 1
	for (j in 1:dim(tab)[1])
	{
		while(mymean+size+upshift<=mymean-downshift-size)
		{
			upshift <- upshift + shift
			downshift <- downshift + shift
		}

		if (tab$bid[j] == mymean + size + upshift)
		{
			vec <- SmartSell(mymean + size - rent + upshift, assets)
			cash <- cash + vec[1]
			assets <- assets + vec[2]

			upshift <- upshift + shift

			bidnum <- bidnum + 1

			num <- num + 1

#			if (asknum==0)
#			{
#			###it means that the last touch was also bid
#				upshift <- upshift + shift
#			}
			bidtime <- tab$date[j]
			asknum <- 0
		}	
		else if (tab$ask[j] == mymean - size - downshift)
		{
			vec <- SmartBuy(mymean - size + rent - downshift, assets)
			cash <- cash + vec[1]
			assets <- assets + vec[2]

			downshift <- downshift + shift

			num <- num + 1

			asknum <- asknum + 1
#			if (bidnum==0)
#			{
#				downshift <- downshift + shift
#			}
			asktime <- tab$date[j]
			bidnum <- 0
		}
		while(tab$date[j]-bidtime>delay)
		{
			downshift <- downshift - shift
			bidtime <- bidtime + delay
		}
		while(tab$date[j]-asktime>delay)
		{
			upshift <- upshift - shift
			asktime <- asktime + delay
		}
	}
	pricebid <- tab$bid[dim(tab)[1]] - rent
	priceask <- tab$ask[dim(tab)[1]] + rent
	print("normal")
	print(pricebid)
	print(priceask)
	cashR <- cash + Normalize(cash, assets, pricebid, priceask)
	c(cash, cashR, assets, num)
}
###Not run
Strategy4 <- function(tab, size, shift, mymean, rent=0)
{
	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0
	upshift <- shift
	downshift <- shift
	for (j in 1:dim(tab)[1])
	{
		if (tab$bid[j] == mymean + size + upshift)
		{
			vec <- SmartSell(mymean + size - rent + upshift, assets)
			cash <- cash + vec[1]
			assets <- assets + vec[2]

			upshift <- upshift + shift

			bidnum <- bidnum + 1

			num <- num + 1

			if (asknum==0)
			{
			###it means that the last touch was also bid
				downshift <- downshift - shift
			}
			asknum <- 0
		}	
		else if (tab$ask[j] == mymean - size - downshift)
		{
			vec <- SmartBuy(mymean - size + rent - downshift, assets)
			cash <- cash + vec[1]
			assets <- assets + vec[2]

			downshift <- downshift + shift

			num <- num + 1

			asknum <- asknum + 1
			if (bidnum==0)
			{
				upshift <- upshift - shift
			}
			bidnum <- 0
		}
	}
	pricebid <- tab$bid[dim(tab)[1]] - rent
	priceask <- tab$ask[dim(tab)[1]] + rent
	print("normal")
	print(pricebid)
	print(priceask)
	cashR <- cash + Normalize(cash, assets, pricebid, priceask)
	c(cash, cashR, assets, num)
}
###процедура работы со скачками
###вход: данные, оценка размера скачка снизу, оценка длительности скачка сверху, размер окна
###выход: преобразованные данные
KillJumps <- function(tab, jumpsize, jumpdur)
{
	tabres <- rbind(c(), c())
	days <- unique(tab$day)
	for (day in days)
	{
		print(day)
		tabday <- tab[tab$day == day,]
		spreads <- abs(tabday$ask - tabday$bid)/2
		meanspread <- median(spreads)
		vec <- (tabday$bid + tabday$ask)/2
		mat <- cbind(1:dim(tabday)[1], c(0,abs(diff(vec))), tabday$date)
		matred <- rbind(c(),mat[mat[,2]>=jumpsize,])
		maxchecked <- 0
#		if (dim(matred)[1] > 0)
#		{
		while ((dim(matred)[1] > 0))
		{
			j <- matred[1,1]
				#print(matred[,1])
				#print(j)

				valprev <- vec[j-1]
				valcur <- vec[j]
				vecred <- vec[j:(min(j+jumpdur, length(vec)))]

				if (min(abs(vecred - valprev)) >= jumpsize) {flag <- 0}
				else if ((valcur>valprev)) {flag <- 1}
				else if ((valcur<valprev)) {flag <- -1}

				if (flag != 0)
				{
					###it was a short jump
#				print(c(valprev, valcur))
#				print(vecred)
					print(c(day, j, mat[j,3])) 
					for (k in 1:(length(vecred)))
					{
						if (flag>0) {if (vecred[k] <= valprev + jumpsize) {break}}
						else if (flag<0) {if (vecred[k] >= valprev - jumpsize) {break}}
						tabday$bid[((tabday$day == day) & (tabday$date == mat[j + k - 1,3]))] <- valprev - meanspread
						tabday$ask[((tabday$day == day) & (tabday$date == mat[j + k - 1,3]))] <- valprev + meanspread
					}
					vec <- (tabday$bid + tabday$ask)/2
					mat <- cbind(1:dim(tabday)[1], c(0,abs(diff(vec))), tabday$date)
					matred <- cbind(c(),mat[mat[,2]>=jumpsize,])
				}	
			maxchecked <- j
			matred <- matred[matred[,1] > maxchecked,]
			if (is.null(dim(matred)[1]) == TRUE) {break}
		}		
		tabres <- rbind(tabres, tabday)
	}
	tabres
}

###Эта функция выдает информацию о стандартнх отклонениях (bid+ask)/2
###Вход: N --- размер промежутка, за который берется внутридневное стандартное отклонение.
###Выход: матрица дневное стандартное отклонение и внутридневное стандартное отклонение
###используется скользящее окно
###рассматриваются лишь нечётные дни
###идея функции состоит в том, чтобы подготовить информацию для регрессии
###дневного стандартного отклонения от внутридневного стандартного отклонения
###кроме того ещё выдаётся размер 90%-й трубки
###и максимальный размер отклонения от среднего в этом интервальчике
####GetTube <- function(moment, day, skip, tab, ratio)
####list(kefs=kefs,qq=c(q1,q2), end=kefs[1]+kefs[2]*(moment+1))
GetSD <- function(tab,N)
{
	days <- unique(tab$day)
	v1 <- c()
	v2 <- c()
	v0 <- c()
	v4 <- c()
	v5 <- c()
	v6 <- c()
	v7 <- c()
	v8 <- c()
	v9 <- c()
	cutval <- 15*60
	for (day in days)
	{
		sdprev <- 0
		if (day==0) {next}
		if (day %% 2 == 1) {next}
		if (day %in% c(162, 216, 324)) {next}
	   	print(day)
		tabred <- tab[tab$day == day,]
		datemin <- tabred$date[1]
		datemax <- tabred$date[length(tabred$date)]
		tabred <- tabred[(tabred$date >= datemin + cutval) & (tabred$date <= datemax - cutval),]
		tabred <- (tabred$bid + tabred$ask)/2

		tube <- GetTube(0, day, 1, tab, 0.1)
		mymean <- tube$end
		upper <- tube$qq[2]
		lower <- tube$qq[1]

		if (length(tabred)>2*N)
		{
		k <- 1
		while (k+N<=length(tabred))
		{
 			v0 <- c(v0, day)
			v4 <- c(v4, k)
			v1 <- c(v1, sd(tabred))
			v2 <- c(v2, sd(tabred[k:(k+N)]))	
			v5 <- c(v5, upper)
			v6 <- c(v6, lower)
			v7 <- c(v7, max(tabred[k:(k+N)]-mymean))
			v8 <- c(v8, -min(tabred[k:(k+N)]-mymean))
			v9 <- c(v9, sdprev)
			sdprev <- sd(tabred[k:(k+N)])
			k <- k + N
		}
		}
	}
	data.frame(day=v0, interval=v4, sdglobal=v1, sdlocal=v2, upper=v5, lower=v6, upjump=v7, lowjump=v8, sdprev=v9)
}

FindExtraDayDollarParams <- function(tab, sizes1, sizes2, shifts1, shifts2, rent, str)
{
for (size1 in sizes1)
{
	for (size2 in sizes2)
	{
		for (shift1 in shifts1)
		{
			for (shift2 in shifts2)
			{
#######				shift2 <- shift1; size2 <- size1
				params <- c(size1, shift1, shift2, size2)
				print(params)
				temp <- ExtraDayMainStrategy(tab, params, rent, 0)
				line <- rbind(c(),c(size1, shift1, shift2, size2, mean(temp$cashvec), length(temp$deals[temp$deals>0])/length(temp$deals), mean(temp$cashvec)/sd(temp$cashvec)))
				####print(line)
				write.table(line, str, append=TRUE, row.names=FALSE, col.names=FALSE)
			}
		}		
	}
}
}

#library(doParallel)
#library(foreach)

ParallelInit <- function()
{
	num <- detectCores()
	cl <- makeCluster(num)
	registerDoParallel(cl)
	getDoParWorkers()
}

###CallMe <- function(i, sizes, shifts, delays, Ns)

ParComp <- function(tab, sizes, shifts, delays, Ns, str, mode="odd", rent)
{
	len <- length(sizes)
	x <- foreach(i = 1:8) %dopar%
	{
		print(paste0(str, i, ".txt"))
		if (i<8)
		{
		#	print( c(1 + floor((i-1)*len/8), 1 + floor(i*len/8) ) );
			source("Strategies.r")
			StrategiesMA(tab, sizes[(1 + floor((i-1)*len/8)):(1 + floor(i*len/8)) ], shifts, delays, Ns,c(1), paste0(str, i, ".txt"), mode, rent)
		}
		else
		{
			source("Strategies.r")
		#	print(c(1 + floor((i-1)*len/8), len);
			StrategiesMA(tab, sizes[(1 + floor((i-1)*len/8)):(len) ], shifts, delays, Ns, c(1), paste0(str, i, ".txt"), mode, rent)
		}
	}	
}
####StrategiesMA <- function(tab, sizes, shifts, delays, Ns, uplims, str, mode="odd", rent)
###Рамка, построенная вокруг скользящего среднего
###стратегия написана для maxposa>1 (другой учёт maxposa) и для разных отклонений от центра вверх и вниз
###Выход: вектор
###максимальная открытая позиция, p&l
######количество активов (со знаком) до закрытия 
###количество сделок без учета закрытия
###максимальная задействованная сумма (без учета ГО)
Strategy9 <- function(tab, size1, size2, shift1, shift2, delay, N, rent, uplim)
{
	####print(c(size1, size2, shift1, shift2))	

	stoploss <- -0.3
	waitparam <- 60
	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0

	maxlot <- 0
	maxcash <- 0
	bidprev <- min(tab$bid[N+2], tab$ask[N+2])
	askprev <- max(tab$ask[N+2], tab$bid[N+2])
	modshift1 <- 0
	modshift2 <- 0

	broken <- FALSE
	baddate <- 0	

	eps <- 0.000001

	debugtab <- c()

####	for (j in (1):dim(tab)[1])
	for (j in (N+2):dim(tab)[1])
	{

		if (tab$date[j] - tab$date[1]>30600) {broken <- TRUE; index <- j; break}

		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 
		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}


		#if (j==(N+2))
		#	{mymean <- MovingAverage(N, tab, j-1)}
		#else
		#	{
		#		mymean <- mymean + (-tab$bid[j-N-2]-tab$ask[j-N-2]+tab$bid[j-1]+tab$ask[j-1])/(2*(N+1))
		#	}

		mymean <- MovingAverage(N, tab, j-1)

#if (j-N-1==285)
#{print(paste("???", j-N-1, bidcur, askcur, mymean-size2+modshift2, mymean+size1+modshift1)); print(c(assets, modshift1, modshift2))}


		if (bidcur > mymean + size1 + modshift1 - eps)
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>0)))
			{
				#print(paste("sell", j-N-1, bidcur, askcur, mymean-size2+modshift2, mymean+size1+modshift1))			
	
				possibpos <- ceiling((bidcur - (mymean + size1 + modshift1 - eps))/shift1)
				if (possibpos > max(uplim + assets,0)) {possibpos <- max(uplim + assets,0)}
				while (possibpos > 0)
				{
					#print(paste(possibpos, "bid", assets))
					#print(c(size1, size2, shift1, shift2))
					pricebid <- mymean + size1 + modshift1
					vec <- Sell(pricebid - rent, 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					bidnum <- bidnum + 1
write.table(rbind(c(),c(10, size1, shift1, size2, shift2, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)

					modshift1 <- modshift1 + shift1
					modshift2 <- modshift2 + shift2
					possibpos <- possibpos - 1
					break
				}
			}
		}
		else if (askcur < mymean - size2 + modshift2 + eps)
		{
			if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<0)))
			{
				#print(paste("buy", j-N-1, bidcur, askcur, mymean-size2+modshift2, mymean+size1+modshift1, bidnum, asknum))			

				possibpos <- ceiling((mymean - size2 + modshift2 + eps - askcur)/shift2)

				#print(paste("possibpos", possibpos))

				if (possibpos > max(uplim - assets,0)) {possibpos <- max(uplim - assets,0)}
				while (possibpos > 0)
				{
					#print(paste(possibpos, "ask", assets))
					#print(c(size1, size2, shift1, shift2))
					priceask <- mymean - size2 + modshift2
					vec <- Buy(priceask + rent, 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					asknum <- asknum + 1
write.table(rbind(c(),c(10, size1, shift1, size2, shift2, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)

					modshift1 <- modshift1 - shift1
					modshift2 <- modshift2 - shift2
					possibpos <- possibpos - 1 
					break
				}
			}
		}

		maxlot <- max(maxlot,abs(assets))
		maxcash <- min(maxcash, cash)

		bidprev <- bidcur
		askprev <- askcur
	}
	if (broken) {index <- j} else {index <- dim(tab)[1]}
	if (index!=dim(tab)[1]) {print(c(tab$day[j],j)); print(">>>>")}

	pricebid <- min(tab$bid[index], tab$ask[index])
	priceask <- max(tab$bid[index], tab$ask[index])
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	maxcash <- -maxcash

	if (assets<0)
	{
write.table(rbind(c(),c(10, size1, shift1, size2, shift2, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
	}
	else if (assets>0)
	{
	write.table(rbind(c(),c(10, size1, shift1, size2, shift2, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
	}
	c(maxlot, cashR, assets, num, maxcash)
}
###эта версия процедура работает с секундными данными, а вычисляет скользящее среднее по минутам
Strategy9mod <- function(tab, size1, size2, shift1, shift2, delay, N, rent, uplim)
{
	####print(c(size1, size2, shift1, shift2))	
	vecred <- seq(1, dim(tab)[1], 60)
	tabred <- tab[vecred,]

	stoploss <- -100
	waitparam <- 60
	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0

	maxlot <- 0
	maxcash <- 0
	bidprev <- min(tab$bid[(N+1)*60], tab$ask[(N+1)*60])
	askprev <- max(tab$ask[(N+1)*60], tab$bid[(N+1)*60])
	modshift1 <- 0
	modshift2 <- 0

	broken <- FALSE
	baddate <- 0	

	eps <- 0.000001

	debugtab <- c()

	for (j in ((N+1)*60 + 1):dim(tab)[1])
	{
		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 
		if (StopLossCheck(stoploss, bidcur, askcur, cash, assets, rent) == TRUE) 
		{
			if (broken==FALSE)
			{
				broken <- TRUE
				baddate <- tab$date[j]
			}
			else
			{

				if (tab$date[j] - baddate > waitparam)
				{
					break
				}
			}
		}
		else {broken <- FALSE}


#####		if (((j-1) %/% 60)==(N+1))
		mymean <- MovingAverage(N, tabred, ((j-1) %/% 60))

		#else
		#	{
		#		mymean <- mymean + (-tabred$bid[((j-1) %/% 60)-N-1]-tabred$ask[((j-1) %/% 60)-N-1]+tabred$bid[((j-1) %/% 60)]+tabred$ask[((j-1) %/% 60)])/(2*(N+1))
		#	}

		###print(mymean)
		#####write.table(mymean,"log.txt")

#if (j-N-1==285)
#{print(paste("???", j-N-1, bidcur, askcur, mymean-size2+modshift2, mymean+size1+modshift1)); print(c(assets, modshift1, modshift2))}


		if (bidcur > mymean + size1 + modshift1 - eps)
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>=0)))
			{
				print(paste("sell", j-N-1, bidcur, askcur, mymean-size2+modshift2, mymean+size1+modshift1))			
	
				possibpos <- ceiling((bidcur - (mymean + size1 + modshift1 - eps))/shift1)
				if (possibpos > max(uplim + assets,0)) {possibpos <- max(uplim + assets,0)}
				while (possibpos > 0)
				{
					#print(paste(possibpos, "bid", assets))
					#print(c(size1, size2, shift1, shift2))
					pricebid <- mymean + size1 + modshift1
					vec <- Sell(pricebid - rent, 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					bidnum <- bidnum + 1
write.table(rbind(c(),c(9, size1, shift1, size2, shift2, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)

					modshift1 <- modshift1 + shift1
					modshift2 <- modshift2 + shift2
					possibpos <- possibpos - 1
					break
				}
			}
		}
		else if (askcur < mymean - size2 + modshift2 + eps)
		{
			if ((asknum - bidnum < uplim) | ((broken==TRUE) & (assets<=0)))
			{
				print(paste("buy", j-N-1, bidcur, askcur, mymean-size2+modshift2, mymean+size1+modshift1, bidnum, asknum))			

				possibpos <- ceiling((mymean - size2 + modshift2 + eps - askcur)/shift2)

				#print(paste("possibpos", possibpos))

				if (possibpos > max(uplim - assets,0)) {possibpos <- max(uplim - assets,0)}
				while (possibpos > 0)
				{
					#print(paste(possibpos, "ask", assets))
					#print(c(size1, size2, shift1, shift2))
					priceask <- mymean - size2 + modshift2
					vec <- Buy(priceask + rent, 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
					num <- num+1
					asknum <- asknum + 1
write.table(rbind(c(),c(9, size1, shift1, size2, shift2, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)

					modshift1 <- modshift1 - shift1
					modshift2 <- modshift2 - shift2
					possibpos <- possibpos - 1 
					break
				}
			}
		}

		maxlot <- max(maxlot,abs(assets))
		maxcash <- min(maxcash, cash)

		bidprev <- bidcur
		askprev <- askcur
	}
	if (broken) {index <- j} else {index <- dim(tab)[1]}
	if (index!=dim(tab)[1]) {print(c(tab$day[j],j)); print(">>>>")}

	pricebid <- min(tab$bid[index], tab$ask[index])
	priceask <- max(tab$bid[index], tab$ask[index])
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	maxcash <- -maxcash

	if (assets<0)
	{
write.table(rbind(c(),c(9, size1, shift1, size2, shift2, N, 1, priceask, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
	}
	else if (assets>0)
	{
	write.table(rbind(c(),c(9, size1, shift1, size2, shift2, N, -1, pricebid, tab$day[1], tab$date[j])), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
	}
	c(maxlot, cashR, assets, num, maxcash)
}

GetStavka <- function(str1, str2, day)
{
	tab1 <- read.table(str1, header=TRUE, sep=",")
	tab2 <- read.table(str2, header=TRUE, sep=",")

	print(summary(tab1))
	print(summary(tab2))

	vec <- 1:dim(tab1)[1]
	tab1 <- cbind(vec, tab1$B1, tab1$A1)
	tab2 <- cbind(vec, tab2$B1, tab2$A1)
	bids <- (tab2[,2]/1000 - tab1[,3])*200/(tab1[,2] + tab1[,3])
	asks <- (tab2[,3]/1000 - tab1[,2])*200/(tab1[,2] + tab1[,3])
	data.frame(day=day, date=vec, bid=bids, ask=asks, periodnum=1)
}

PrepareTabs <- function(str1, str2, day)
{
	tab1 <- read.table(str1, header=TRUE, sep=",")
	tab2 <- read.table(str2, header=TRUE, sep=",")

	print(summary(tab1))
	print(summary(tab2))

	vec <- 1:dim(tab1)[1]
	tab1 <- cbind(day, vec, (tab1$B1+tab1$A1)/2, tab1$B1, tab1$A1)
	tab2 <- cbind(day, vec, (tab2$B1+tab2$A1)/2, tab2$B1, tab2$A1)
	list(tab1=tab1, tab2=tab2)
}

ClosingChecker <- function()
{
	restab <- rbind(c())	
	for (i in seq(5,60,5))
	{
		for (j in c(0,1,2))
		{	
			res <- MainStrategy(tabmod, 0.13, 0.1, 1, 77, 0.04, "even", 1, 240, c(i,j))
			restab <- rbind(restab, c(i, j, min(res[,2]), mean(res[,2]), max(res[,2])))
		}
	}
	restab
}
InitializeData <- function(str1, str2, symb, head, flag)
{
	tab1 <- read.table(str1, header=head, sep=symb)
	tab2 <- read.table(str2, header=head, sep=symb)
	tab1[,1] <- as.POSIXct(tab1[,1])
	tab2[,1] <- as.POSIXct(tab2[,1])	

        tabb1 <- tab1[tab1[, 1] %in% tab2[, 1], ]
        tabb2 <- tab2[tab2[, 1] %in% tab1[, 1], ]
        tab1 <- tabb1
        tab2 <- tabb2

	if (flag==1)
	{
		bids <- (tab2[,2]/1000 - tab1[,3])*200/(tab1[,2] + tab1[,3])
		asks <- (tab2[,3]/1000 - tab1[,2])*200/(tab1[,2] + tab1[,3])
	} else {
		bids <- (tab2[,2]/1000 - tab1[,2])*200/(tab1[,2] + tab1[,2])
		asks <- (tab2[,2]/1000 - tab1[,2])*200/(tab1[,2] + tab1[,2])
	}
	data.frame(day=1, date=as.numeric(tab1[,1]) - as.numeric(tab1[1,1]), bid=bids, ask=asks, periodnum=1)
}
InitializeData2 <- function(str1, str2)
{
	tab1 <- read.table(str1, header=FALSE, sep=",")
	tab2 <- read.table(str2, header=FALSE, sep=",")
	tab1[,1] <- as.POSIXct(tab1[,1])
	tab2[,1] <- as.POSIXct(tab2[,1])

	vec <- as.POSIXlt(tab1[,1])$mday

	tab1 <- cbind(tab1, vec)

	print(summary(tab1))
	
	dates <- intersect(as.POSIXct(tab1[,1]), as.POSIXct(tab2[,1]))

	dates <- unique(dates)
	dates <- dates[order(dates)]

	tab1 <- tab1[tab1[,1] %in% dates,]
	tab2 <- tab2[tab2[,1] %in% dates,]

	print(summary(tab1))
	print(summary(tab2))

	print(dim(tab1))
	print(dim(tab2))
	print(length(dates))


	bids <- (tab2[,2]/1000 - tab1[,2])*200/(tab1[,2] + tab1[,2])
	asks <- (tab2[,2]/1000 - tab1[,2])*200/(tab1[,2] + tab1[,2])

	print(summary(dates))

	###days <- as.POSIXlt(as.POSIXct(dates))$mday
	tabb <- data.frame(day=tab1[,8], date=dates, bid=bids, ask=asks, periodnum=1)
	days <- unique(tabb$day)
	for (day in days)
	{
		val = min(as.numeric(tabb$date[tabb$day==day]))
		tabb$date[tabb$day == day] <- as.numeric(tabb$date[tabb$day==day])-val
	}
	tabb
}
MainAvral <- function(tab, deals, vec1, vec2)
{
	restab <- rbind(c())
	for (takeprofit in vec1)
	{
		for (stoploss in vec2)	
		{
			temp <- Avral(tab, deals, takeprofit, stoploss, 0)
			restab <- rbind(restab, rbind(c(takeprofit, stoploss, sum(temp), length(temp[temp>0])/length(temp))))	
	
			print(restab)		

		}
	}
restab
}


Avral <- function(tab, deals, takeprofit, stoploss, rent)
{
	dateprev <- tab[1,1]
	j <- 1
	cashR <- c()

	for (j in 1:dim(deals)[1])
	{
		#print(j)

		cash <- 0; assets <- 0
		tabred <- tab[tab[,1] >= deals[j,1],]
		dateprev <- deals[j,1]
		###tabred <- tab

	if (deals[j,3]>0)
				{
					vec <- Buy(deals[j,2], 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
				}
				else if (deals[j,3]<0)
				{
					vec <- Sell(deals[j,2], 1)
					cash <- cash + vec[1]
					assets <- assets + vec[2]
				}


print("tostart")
if (dim(tabred)[1]>3)
{
		for (i in 1:(dim(tabred)[1]))
		{
			price <- tabred[i,5]
			priceprev <- tabred[i-1,2]
	print(i)
print(price)
			if (StopLossCheck(stoploss, price, price, cash, assets, rent) == TRUE)
			{

				cashR <- c(cashR, cash+Normalize(cash, assets, price, price))
				print(c("sell", j, cash+Normalize(cash, assets, price, price)))
				break
			}
			else if (TakeProfitCheck(takeprofit, price, price, cash, assets, rent) == TRUE)
			{
				print("2222")		

				cashR <- c(cashR, cash+Normalize(cash, assets, price, price))
print(c("buy", j, cash+Normalize(cash, assets, price, price)))
				break
			}

			#if ((deals[j,1] >= dateprev) & (deals[j,1] < tabred[i,1]))
			#{
			#	print("hrere")				
			#
			#}			}
	
		dateprev <- tabred[i,1]
		}
		####break
}
	}
	cashR
}

TakeProfitCheck <- function(takeprofit, pricebid, priceask, cash, assets, rent)
{
####rent <- 3.6
	
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	if (cashR > takeprofit) {res <- TRUE}
	else {res <- FALSE}
	res
}
StopLossCheck <- function(stoploss, pricebid, priceask, cash, assets, rent)
{
####rent <- 3.6	

	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	if (cashR < stoploss) {res <- TRUE}
	else {res <- FALSE}
	res
}
GetFinamData <- function()
{
	str0 <- "candles_TimeFrameCandle_01_00_00_SANDP-500@FINAM_2014_09_02"
}

Glue <- function(tab1, tab2)
{
	tab3 <- tab2
	for (j in 1:dim(tab2)[1])
	{
		print(j)
		tabred <- tab1[tab1$date<=tab2$date[j],]
		tab3$valtalt[j] <- tabred$val[dim(tabred)[1]]		
	}
	tab3
}

StratsOnControl <- function(tab, str)
{
	for (c1 in seq(60,5,-5))
	{
		for (c2 in 0:2)
		{
			res <- MainStrategy(tab=tabmod, size=0.13, shift=0.06, delay=1, idnum=77, rent=0.04, mode="odd", uplim=1, N=240, control=c(c1, c2))
			write.table(rbind(c(), c(c1, c2, sum(res[,2]))), str, append=TRUE,row.names=FALSE,col.names=FALSE)
		}
	}
}

MakeStavka <- function(vecstock, vecfutur)
{
	val1 <- (vecfutur[1]/1000 - vecstock[2])*200/(vecstock[1] + vecstock[2])
	val2 <- (vecfutur[2]/1000 - vecstock[1])*200/(vecstock[1] + vecstock[2])
c(val1, val2)
}
