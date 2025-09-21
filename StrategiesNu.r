###требования к таблице
###дата-фрейм со столбцами
###$day --- номер дня (первый день 0 --- исключается из рассмотрения из-за неполноты данных)
###$date --- дата внутри дня (число от 0 до 86400)
###$bid
###$ask

###процедура получения данных по ставкам
###вход: номер стратегии, параметры стратегии
###выход: дата-фрейм вида: $sign, $quantity, $day, $date
GetDealsData <- function()
{
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname="postgres", user="postgres", password="123")
	str <- "select \"sign\", \"quantity\", \"day\", \"date\" from \"Deals\" where \"nnstrat\"=2 order by \"day\", \"date\""
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
				points(dealsred$date[j], dealsred$quantity[j], pch=24, col="green", cex=3)
			}
			else if (dealsred$sign[j]<0)
			{
				points(dealsred$date[j], dealsred$quantity[j], pch=25, col="red", cex=3)
			}
		}
	}

}

library(RPostgreSQL)
###функция получения данных
GetData <- function()
{
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname="postgres", user="postgres", password="123")
	str <- "select \"Daynum\" as day, \"Mytime\" as date, \"SBERbid\" as ask, \"SBERask\" as bid, \"ExpDays\" as ExpDays from \"StavkaKaramba\" order by \"Daynum\" asc, \"Mytime\" asc"
	tab <- dbGetQuery(con,str)
	tab
}
###процедура тестирования на периодах
###MainStrategy <- function(tab, size, shift=NA, delay=NA, idnum=1, rent=0, mode="odd", uplim=25, N, control)
PeriodTest <- function(size, shift, delay, N, control1, control2, rent)
{
datum<- c()
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
		tval <- c()
		res <- ExtraDayMainStrategy(mytab1, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(1,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
		res <- ExtraDayMainStrategy(mytab2, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(2,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
		res <- ExtraDayMainStrategy(mytab3, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(3,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
		res <- ExtraDayMainStrategy(mytab4, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(4,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
		res <- ExtraDayMainStrategy(mytab5, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(5,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
		res <- ExtraDayMainStrategy(mytab6, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(6,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
		res <- ExtraDayMainStrategy(mytab7, c(size, shift), rent, control=control2)
		datum <- rbind(datum, cbind(7,c(res$cashstate[1],diff(res$cashstate)), res$deals, c(res$currentstate[1],diff(res$currentstate)), res$transfer))
		val <- c(val, res$cash)
		tval <- c(tval, sum(res$transfer))
	}
list(val=val, datum=datum, tval=tval)
}
###процедура работы с внедневными стратегиями на тестовой выборке
CheckStrat <- function(size, shift, rent, control=1)
{
	print(c(size, shift))
	if (control==1)
	{
		res1 <- ExtraDayMainStrategy(mytab4, c(size, shift), rent)
		res2 <- ExtraDayMainStrategy(mytab6, c(size, shift), rent)
		res3 <- ExtraDayMainStrategy(mytab7, c(size, shift), rent)
	}
	else
	{
		res1 <- ExtraDayMainStrategy(mytab1, c(size, shift), rent)
		res2 <- ExtraDayMainStrategy(mytab2, c(size, shift), rent)
		res3 <- ExtraDayMainStrategy(mytab3, c(size, shift), rent)
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
	for (shift in shifts)
	{
		for (size in sizes)
		{
			if (shift>2*size-1) {next}
			print(c(size, shift))
			res1 <- ExtraDayMainStrategy(mytab1, c(size, shift), rent, 0)
			res2 <- ExtraDayMainStrategy(mytab3, c(size, shift), rent, 0)
			res3 <- ExtraDayMainStrategy(mytab6, c(size, shift), rent, 0)
			####ressum <- res1$cash + res2$cash + res3$cash
			write.table(rbind(c(),c(size, shift, res1$cash, res2$cash, res3$cash, sum(res1$transfer), sum(res2$transfer), sum(res3$transfer), sum(res1$deals), sum(res2$deals), sum(res3$deals))), str, append=TRUE, row.names=FALSE, col.names=FALSE)
		}
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
ExtraStrategy <- function(tab, rent, mymean, params, assets, needtoclose, control, uplim=1)
{

	###needtoclose <- TRUE
	###mymean <- (tab$bid[1] + tab$ask[1])/2

	size <- params[1]; shift <- params[2]

	num <- 0
	if (abs(assets)>1) {print("WTF???")}
	if (assets>0)
	{
		asknum <- 1
		bidnum <- 0		
	}
	else if (assets<0)
	{
		bidnum <- 1
		asknum <- 0
	}
	else 
	{
		bidnum <- 0
		asknum <- 0
	}
	modshift <- 0
	uplim <- 1
	cash <- 0

	bidprev <- min(tab$bid[1], tab$ask[1])
	askprev <- max(tab$ask[1], tab$bid[1])

	for (j in 1:dim(tab)[1])
	{

		bidcur <- min(tab$bid[j], tab$ask[j])
		askcur <- max(tab$bid[j], tab$ask[j]) 

		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidcur>mymean+size+modshift) & (bidprev<mymean+size+modshift)))
		{
			if (bidnum - asknum < uplim)
			{
				if (control==1)
				{
					pricebid <- max(bidcur, mymean + size + modshift)
				}
				else
				{
					pricebid <- mymean + size + modshift
				}
				vec <- Sell(pricebid - rent, 1)

			if (withlog)
			{
		write.table(rbind(c(),c(1, size, shift, 0, -1, pricebid, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
			}

				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				bidnum <- bidnum + 1
			}

		}

		if ((abs(askcur - mymean + size - modshift)<0.0000001) | ((askcur<mymean-size+modshift) & (askprev<mymean-size+modshift)))
		{
			if (asknum - bidnum < uplim)
			{
				if (control==1)
				{
					priceask <- min(askcur, mymean - size +  modshift)
				}
				else
				{
					priceask <- mymean - size +  modshift
				}
				vec <- Buy(priceask + rent, 1)

			if (withlog)
			{
		write.table(rbind(c(),c(1, size, shift, 0, 1, priceask, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
			}

				cash <- cash + vec[1]
				assets <- assets + vec[2]
				num <- num+1
				asknum <- asknum + 1
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

	if (needtoclose)
	{
		pricebid <- bidcur
		priceask <- askcur

		if (assets>0)
		{
			write.table(rbind(c(),c(1, size, shift, 0, -assets, pricebid, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
		}
		else if (assets<0)
		{
			write.table(rbind(c(),c(1, size, shift, 0, -assets, priceask, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
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
	uplim <- 1
	print(c(control, uplim))
	days <- unique(tab$day)
	skip <- 1
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
			deals <- c(deals, 0)
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

#			if (control==1)
#			{
				meanvecred <- meanvec
				expdaysred <- expdays
#			}
#			else
#			{
#				meanvecred <- meanvec[(length(meanvec)-skip):(length(meanvec)-skip+1)]
#				expdaysred <- expdays[(length(meanvec)-skip):(length(meanvec)-skip+1)]
#			}
#
#			if (control==1)
#			{
#				mymean <- meanvec[length(meanvec)]
#			}
#			else
#			{
				mymean <- IdentifyMean(meanvecred, expdaysred, tabred$expdays[1])
#			}			

			mymeanvec <- c(mymeanvec, mymean)

			if (j<length(days))
				{res <- ExtraStrategy(tabred, rent, mymean, params, assets, FALSE, control=control, uplim=uplim)}
			else
				{res <- ExtraStrategy(tabred, rent, mymean, params, assets, TRUE, control=control, uplim=uplim)}

			cash <- cash + transfercost*(dayprev - tabred$expdays[1]) + res$cash

#			print(cash)
#			print(cashstate)
#			print(assets)
#			if (day>=10) {break}

			assets <- res$assets
			deals <- c(deals, res$num)
			cashvec <- c(cashvec, res$cash)
			transfer <- c(transfer, transfercost*(dayprev - tabred$expdays[1]))


			####проводим проверку текущей ситуации по последней цене за день
			pricebid <- median(pmin(tabred$bid, tabred$ask)) 
			priceask <- median(pmax(tabred$bid, tabred$ask))
			currentstate <- c(currentstate, cash + Normalize(cash, assets, pricebid-rent, priceask+rent))
			cashstate <- c(cashstate, cash)


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
MakeReport <- function(tab,size, shift, delay, N, idnum, rent)
{
	resodd <- MainStrategy(tab, size, shift, delay, idnum, rent, "odd", 1, N, 0)
	resodd <- as.data.frame(resodd)
	names(resodd) <- c("maxlot", "pl", "assets", "num", "maxcash","sdprev","sdcur","day")
	reseven <- MainStrategy(tab, size, shift, delay, idnum, rent, "even", 1, N, 0)
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
	profs <- c(sum(tab1$pl), sum(tab2$pl))
	maxlots <- c(max(tab1$maxlot), max(tab2$maxlot))
	profsperlot <- c( (sum(tab1$pl) /2200)*(250/dim(tab1)[1]), 
		(sum(tab2$pl)/2200)*(250/dim(tab1)[1]))
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

###Эта функция выдает информацию о стандартнх отклонениях bid
###Вход: N --- размер промежутка, за который берется внутридневное стандартное отклонение.
###Выход: матрица дневное стандартное отклонение и внутридневное стандартное отклонение
###используется скользящее окно
###рассматриваются лишь нечётные дни
###идея функции состоит в том, чтобы подготовить информацию для регрессии
###дневного стандартного отклонения от внутридневного стандартного отклонения
GetSD <- function(tab,N)
{
	days <- unique(tab$day)
	v1 <- c()
	v2 <- c()
	cutval <- 15*60
	for (day in days)
	{
		if (day==0) {next}
		if (day %% 2 == 1) {next}
	   	print(day)
		tabred <- tab[tab$day == day,]
		datemin <- tabred$date[1]
		datemax <- tabred$date[length(tabred$date)]
		tabred <- tabred[(tabred$date >= datemin + cutval) & (tabred$date <= datemax - cutval),]
		tabred <- tabred$bid
		if (length(tabred)>2*N)
		{
		for (k in 1:(length(tabred) - N))
		{
			v1 <- c(v1, sd(tabred))
			v2 <- c(v2, sd(tabred[k:(k+N)]))	
		}
		}
	}
	cbind(v1, v2)
}

###Эта функция рисует графики
###Вход: day --- номер дня, N --- период скользящего среднего
###Рисуется график bid, ask (красным) и скользящего среднего (синим)
###на данный момент скользящее среднее закомменчено
DrawGraph <- function(tab, day, N)
{
	tabred <- tab[tab$day==day,]	

	print(dim(tabred))

####	vec <- MovingAverageCalc(N, tabred)
	#####tabred <- tabred[(N+1):dim(tabred)[1],]
	plot(tabred$date, tabred$bid, type="l")
	lines(tabred$date, tabred$ask, col="red")	
####	lines(tabred$date[(N+1):dim(tabred)[1]], vec, col="blue")
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
StrategiesMA <- function(tab, sizes, shifts, delays, Ns, str, mode="odd")
{
	for (size in sizes)
	{
	for (shift in shifts)
	{
if (shift>2*size-1) {next}
	for (delay in delays)
	{
	for (N in Ns)
	{
		if ((shift==0) & (delay>1)) {break}
		res <- MainStrategy(tab, size=size, shift=shift, delay=delay, rent=2.6, mode=mode, idnum=7, uplim=1, N=N, control=0)
		line <- rbind(c(),c(size, shift, delay, N, mean(res[,2]), mean(res[,4]), mean(res[,6]), median(res[,2]), mean(res[,2])/sd(res[,2])))
		write.table(line,str,append=TRUE,row.names=FALSE,col.names=FALSE)
	}
	}
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
MainStrategy <- function(tab, size, shift=NA, delay=NA, idnum=1, rent=0, mode="odd", uplim=25, N, control)
{
	print(mode)
	print(c(rent, uplim, control))
	days <- unique(tab$day)
tabdayprev <- c()
	resdata <- data.frame(v1=c(), v2=c(), v3=c(), v4=c(), v5=c(), v6=c())
	if (mode=="odd")
		{
		days <- days[2:length(days)]
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
				resdata <- rbind(resdata, c(AnalyzeDay(tab=tabday, tabprev=tabdayprev, size=size, shift=shift, delay=delay, idnum=idnum, rent=rent, uplim=uplim, N=N, control=control),day))
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
				resdata <- rbind(resdata, c(AnalyzeDay(tab=tabday, tabprev=tabdayprev, size=size, shift=shift, delay=delay, idnum=idnum, rent=rent, uplim=uplim, N=N, control=control),day))
				tabdayprev <- tabday
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
AnalyzeDay <- function(tab, tabprev, size, shift=NA, delay=NA, idnum=1, rent=0, uplim=25, N=0, control)
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
	mymean <- CalculateMean(tabprev) 
	sdprev <- sd(tabprev$bid)
	sdcur <- sd(tab$bid)		
	if (is.na(sdcur)==TRUE) {sdcur <- 0}
	if (is.na(sdprev)==TRUE) {sdprev <- 0}
	resdata <- c(Strategy1(tab, size, mymean, rent, uplim=uplim), sdprev, sdcur)
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

		if ((abs(bidcur - mymean - size - modshift)<0.000001) | ((bidprev < mymean + size + modshift) & (bidcur>mymean+size+modshift)))
		{
			if ((bidnum - asknum < uplim) | ((broken==TRUE) & (assets>=0)))
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

		####	write.table(rbind(c(),c(control, size, shift, N, tab$day[1], tab$date[j], -1, pricebid)), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
			if (withlog)
			{
		write.table(rbind(c(),c(2, size, shift, N, -1, pricebid, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
			}


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
				if (control==1)
				{
					priceask <- min(askcur, mymean - size +  modshift)
				}
				else
				{
					priceask <- round(mymean - size + modshift)
				}
				vec <- Buy(priceask + rent, 1)

#####			write.table(rbind(c(),c(control, size, shift, N, tab$day[1], tab$date[j], 1, priceask)), "logex.txt", append=TRUE, row.names=FALSE, col.names=FALSE)
			if (withlog)
			{
		write.table(rbind(c(),c(2, size, shift, N, 1, priceask, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
			}


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


		if (assets>0)
		{
			write.table(rbind(c(),c(2, size, shift, N, -assets, pricebid, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
		}
		else if (assets<0)
		{
			write.table(rbind(c(),c(2, size, shift, N, -assets, priceask, tab$day[j], tab$date[j])), "log.txt", sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
		}


	maxcash <- -maxcash
	c(maxlot, cashR, assets, num, maxcash)
####	c(maxlot, cash, assets, num, maxcash)
}
###функция проверки на stoploss
###выход: TRUE --- stoploss, FALSE --- всё в порядке
StopLossCheck <- function(stoploss, pricebid, priceask, cash, assets, rent=3)
{
####rent <- 3.6
	
	cashR <- cash + Normalize(cash, assets, pricebid-rent, priceask+rent)
	if (cashR < stoploss) {res <- TRUE}
	else {res <- FALSE}
	res
}
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
###Not run
Strategy1 <- function(tab, size, mymean, rent=0, uplim=25)
{
	cash <- 0
	assets <- 0
	num <- 0
	bidnum <- 0
	asknum <- 0
	bidprev <- tab$bid[1]
	askprev <- tab$ask[1]
	for (j in 1:dim(tab)[1])
	{
		if ((tab$bid[j] == mymean + size) | 
			((tab$bid[j] > mymean + size) & (bidprev < mymean + size)))
		{
		   if (bidnum < uplim)
		   {
			pricebid <- max(tab$bid[j], mymean + size)
			vec <- SmartSell(pricebid - rent, assets)
			cash <- cash + vec[1]
			assets <- assets + vec[2]
			num <- num+1
#			print("sell")
#			print(tab$bid[j])
#			print("h1")
#			print(as.numeric(mymean + size - rent))
#			print("h2")
#			print("sell-cash")
#			print(cash)
#			print("sell-assets")
#			print(assets)
#			print(bidnum)
#			print("sell-end")
			bidnum <- bidnum + 1
			asknum <- 0
                  }
		}

		if ((tab$ask[j] == mymean - size) | 
			((askprev > mymean - size) & (tab$ask[j] < mymean - size)))
		{
                   if (asknum < uplim)
                   {
			priceask <- min(tab$ask[j], mymean - size)
			vec <- SmartBuy(priceask + rent, assets)
			cash <- cash + vec[1]
			assets <- assets + vec[2]
			num <- num+1
#			print("buy")
#			print(tab$ask[j])
#			print(mymean - size + rent)
#			print("buy-cash")
#			print(cash)
#			print("buy-assets")
#			print(assets)
#			print(asknum)
#			print("buy-end")
                        asknum <- asknum + 1
                        bidnum <- 0
                  }
		}
		bidprev <- tab$bid[j]
		askprev <- tab$ask[j]

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

