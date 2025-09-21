###library(RPostgreSQL)

TestUniformityForGroups <- function(tabpos, tabneg, tab)
{
	groups <- unique(tabpos$group)
	groups <- groups[groups %in% tabneg$group]
	groups <- groups[order(groups)]
	restab <- rbind(c())
	for (group in groups)
	{
		#print(group)
		temp <- TestUniformityForGroup(group, tabpos, tabneg, tab)
		print(c(group, length(temp$valpos), length(temp$valneg)))	
		pv <- ks.test(temp$valpos, temp$valneg)$p.value
		###write.table(rbind(c(),c(group, pv, length(temp$valpos), length(temp$valneg))), "chisqresults.txt",append=TRUE,row.names=FALSE,col.names=FALSE)
		print(c(group, pv, length(temp$valpos), length(temp$valneg)))
		restab <- rbind(restab,  c(group, pv, length(temp$valpos), length(temp$valneg)))
	}
restab
}

TestUniformityForGroup <- function(group, tabpos, tabneg, tab)
{
	tabpos <- tabpos[tabpos$group==group,]
	tabneg <- tabneg[tabneg$group==group,]
	valpos <- c()
	valneg <- c()
	#for (i in tabpos$id)
	#{
	#	print(i)
	#	valpos <- c(valpos, tab$valfutur12[tab$id==i] - tab$valend[tab$id==i])
	#}
	valpos <- tab$valfutur12[tab$id %in% tabpos$id] - tab$valend[tab$id %in% tabpos$id]
	#for (i in tabneg$id)
	#{
	#	print(i)
	#	valneg <- c(valneg, tab$valfutur12[tab$id==i] - tab$valend[tab$id==i])
	#}
	valneg <- tab$valfutur12[tab$id %in% tabneg$id] - tab$valend[tab$id %in% tabneg$id]
	list(valpos=valpos, valneg=valneg)
}
IdentifyGroupMini <- function(val, centers)
{
	centersfull <- cbind(1:dim(centers)[1], centers)
	centersfull <- cbind(centersfull, abs(val - centers))
	centersfull <- centersfull[order(centersfull[,3]),]
	centersfull[1]
}
IdentifyGroupFull <- function(j)
{
	val1 <- TAB60$sd[j]
	val2 <- TAB60$trend[j]
	val3 <- TAB60$pos[j]

	num1 <- IdentifyGroupMini(val1, centers1)
	num2 <- IdentifyGroupMini(val2, centers2)
	num3 <- IdentifyGroupMini(val3, centers3)
	num3*100 + num2*10 + num1
}
IdentifyGroup <- function(j)
{
	tab <- TAB60
	tabinfo <- TABINFO60

if (tab$group[j]<0)
{
		sdj <- tab$sd[j]
		trendj <- tab$trend[j]
		shiftj <- tab$shift[j]

		#print(c(sdj, trendj, shiftj))

		info <- tabinfo[((tabinfo$qsdstart <= sdj) & (sdj<tabinfo$qsdend) & (tabinfo$qtrendstart <= trendj) & (trendj<tabinfo$qtrendend)
				& (tabinfo$qshiftstart <=  shiftj) & (shiftj<tabinfo$qshiftend)),]

		####info <- tabinfo[((tabinfo$qsdstart <= sdj) & (sdj<tabinfo$qsdend) & (tabinfo$qtrendstart <= trendj)),]

		#print(info)

		res <- info$groupid
}
c(j, res)
}

IdentifyGroupAlt <- function(j)
{
	tab <- TAB60
	tabinfo <- TABINFO60

if (tab$group[j]>=0)
{
		sdj <- tab$sd[j]
		trendj <- tab$trend[j]
		shiftj <- tab$shift[j]

		#print(c(sdj, trendj, shiftj))

		info <- tabinfo[((tabinfo$qsdstart <= sdj) & (sdj<tabinfo$qsdend) & (tabinfo$qtrendstart <= trendj) & (trendj<tabinfo$qtrendend)
				& (tabinfo$qshiftstart <=  shiftj) & (shiftj<tabinfo$qshiftend)),]

		####info <- tabinfo[((tabinfo$qsdstart <= sdj) & (sdj<tabinfo$qsdend) & (tabinfo$qtrendstart <= trendj)),]

		#print(info)

		res <- info$groupid
}
c(j, res)
}

GetGroupsMod <- function()
{
	TAB60$id <- 1:dim(TAB60)[1]
	vec <- TAB60$id[TAB60$group>=0]
	vecalt <- TAB60$id[TAB60$group<0]

	tabpos <- as.data.frame(t(sapply(as.vector(vec), IdentifyGroupFull)))
	tabneg <- as.data.frame(t(sapply(as.vector(vecalt), IdentifyGroupFull)))

	list(tabpos=data.frame(id=1:length(t(tabpos)), group=t(tabpos)), tabneg=data.frame(id=1:length(t(tabneg)), group=t(tabneg)))
}

GetGroups <- function()
{
	TAB60$id <- 1:dim(TAB60)[1]
	vec <- TAB60$id[TAB60$group>=0]
	vecalt <- TAB60$id[TAB60$group<0]

	####print(length(vec)/length(TAB60$id))
	print(length(vecalt)/length(TAB60$id))	

	tabpos <- as.data.frame(t(sapply(as.vector(vec), IdentifyGroupAlt)))
	tabneg <- as.data.frame(t(sapply(as.vector(vecalt), IdentifyGroup)))

	print(dim(tabpos))
	print(dim(tabneg))

	list(tabpos=tabpos, tabneg=tabneg)
}

InitializationMod <- function()
{
	tab <- read.table("IntervDat60.txt", header=FALSE, sep=",")
	tab <- cbind(tab, 0)
	tab <- as.data.frame(tab)
	names(tab) <- c("intervid",  "datestart", "dateend",   "daystart",  "dayend",    "valstart",  "valend",    "valfutur1",
		"valfutur2", "valfutur3", "valfutur4", "valfutur5", "valfutur6", "valfutur7", "valfutur8", "valfutur9", 
		"valfutur10", "valfutur11", "valfutur12",  "sd",        "trend",     "shift", "pos", "group" )	
	tab <- Erase(tab, "even")
	###MakeGroups(tab,10,10,10)
tab
}

Initialization <- function()
{
	tab <- read.table("IntervDat60.txt", header=FALSE, sep=",")
	tab <- cbind(tab, 0)
	tab <- as.data.frame(tab)
	names(tab) <- c("intervid",  "datestart", "dateend",   "daystart",  "dayend",    "valstart",  "valend",    "valfutur1",
		"valfutur2", "valfutur3", "valfutur4", "valfutur5", "valfutur6", "valfutur7", "valfutur8", "valfutur9", 
		"valfutur10", "valfutur11", "valfutur12",  "sd",        "trend",     "shift", "pos", "group" )	
	tab <- Erase(tab, "even")
	tab <- FillsdGroups(tab)
	tab <- FilltrendsignGroups(tab)
	tab$group <- 10*tab$group
	tab <- FillposGroups(tab)
	tabinfo <- FillGroupInfo(tab)

list(tab=tab, tabinfo=tabinfo)
}

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

###структура restab:
###номер, время начала, время конца, день начала, день конца, значение начала, значение конца, значение через delta, sd, наклон, сдвиг
CalculateIntervals <- function(tab, str, delta, packagesize=10, step=5)
{
	restab <- rbind(c())
	for (j in 1:dim(tab)[1])
	{
		if (j+2*delta > dim(tab)[1]) {break}
		print(j)
		tabred <- tab[(j:(j+delta)),]
		par1 <- sd(tabred$bid)
		tempvec <- 1:(dim(tabred)[1])
		temp <- lm(tabred$bid ~ tempvec)		
		par2 <- as.numeric(temp$coefficients[2])
		par3 <- as.numeric(temp$coefficients[1])
		par4 <- tabred$bid[1+delta] - par2*(1+delta) - par3
		par5 <- sd(temp$residuals)
		vec <- c()
		i <- delta + step
		while (i<=2*delta)
		{
			vec <- c(vec, tab$bid[1 + j + i])
			i <- i + step
		}
		restab <- rbind(restab, rbind(c(), c(j,tabred$date[1], tabred$date[1+delta], tabred$day[1], tabred$day[1+delta], 
					tabred$bid[1], tabred$bid[1+delta], vec, par1,par2,par3,par4,par5)))
		if (dim(restab)[1] >= packagesize)
		{
			write.table(restab, str, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)			
			restab <- rbind(c())
		}
	}
	if (dim(restab)[1] >= 0)
	{
		write.table(restab, str, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)			
		restab <- rbind(c())
	}
}
MakeClusters <- function(tab, cluster1, cluster2, cluster3)
{
	num1 <- length(unique(cluster1)); num2 <- length(unique(cluster2)); num3 <- length(unique(cluster3))
	###считается, что все эти числа меньше 10
	index <- 0
	for (i in 1:dim(tab)[1])
	{
		print(i)
		if (tab$group[i]==0)
		{
			index <-  index + 1
			c1 <- cluster1[index]
			c2 <- cluster2[index]
			c3 <- cluster3[index]
			tab$group[i] <- c3*100 + c2*10 + c1
		}
	}
	groups <- unique(tab$group[tab$group>=0])
	tabinfo <- rbind(c())
	print("groups collected")
	for (group in groups)
	{
		tabinfo <- rbind(tabinfo, rbind(c(group, dim(tab[tab$group==group,])[1])))
	}
list(tab=tab, tabinfo=tabinfo)
}
MakeGroups <- function(tab, num1, num2, num3)
{
	restab <- c()
	groupid <- 0
	
	print("MAKEGROUPS")

	for (i1 in 1:num1)
	{
		q1 <- as.numeric(quantile(tab$sd[tab$group>=0], (i1-1)/num1))
		q11 <- as.numeric(quantile(tab$sd[tab$group>=0], i1/num1))

		if (i1==1) {q1 <- 0}
		else if (i1==num1) {q11 <- 100}

		for (i2 in 1:num2)
		{
			q2 <- as.numeric(quantile(tab$trend[tab$group>=0], (i2-1)/num2))
			q22 <- as.numeric(quantile(tab$trend[tab$group>=0], i2/num2))
 
			if (i2==1) {q2 <- -100}
			else if (i2==num2) {q22 <- 100}

			for (i3 in 1:num3)
			{
				print(c(i1, i2, i3))
				q3 <- as.numeric(quantile(tab$shift[tab$group>=0], (i3-1)/num3))
				q33 <- as.numeric(quantile(tab$shift[tab$group>=0], i3/num3))

				if (i3==1) {q3 <- -100}
				else if (i3==num3) {q33 <- 100}
				
				tabred <- tab[((tab$group>=0) & (tab$sd>=q1) & (tab$sd<q11) & (tab$trend>=q2) & (tab$trend<q22) & (tab$shift>=q3) & (tab$shift<q33)),]
				groupid <- groupid + 1

				print(groupid)

				if (dim(tabred)[1] == 0)
				{
					restab <- rbind(restab, rbind(c(),c(groupid, q1, q11, q2, q22, q3, q33, 0)))
				}
				else
				{
					restab <- rbind(restab, rbind(c(),c(groupid, q1, q11, q2, q22, q3, q33, dim(tabred)[1])))
					tab$group[((tab$group>=0) & (tab$sd>=q1) & (tab$sd<q11) & (tab$trend>=q2) & (tab$trend<q22) & (tab$shift>=q3) & (tab$shift<q33))] <- groupid
				}	
			}
		}
	}
	restab <- as.data.frame(restab)
	names(restab) <- c("groupid", "qsdstart", "qsdend", "qtrendstart", "qtrendend", "qshiftstart", "qshiftend", "quantity")
	list(tab=tab, restab=restab)
}
MakeGroupsMod <- function(tab, num1, num2, num3)
{
	restab <- c()
	groupid <- 0
	tab$group <- groupid
	for (i1 in 1:num1)
	{
		q1 <- as.numeric(quantile(tab$sd[tab$group>=0], (i1-1)/num1))
		q11 <- as.numeric(quantile(tab$sd[tab$group>=0], i1/num1))

		if (i1==1) {q1 <- 0}
		else if (i1==num1) {q11 <- 100}

		for (i2 in 1:num2)
		{
			q2 <- as.numeric(quantile(tab$trend[tab$group>=0], (i2-1)/num2))
			q22 <- as.numeric(quantile(tab$trend[tab$group>=0], i2/num2))

			if (i2==1) {q2 <- -100}
			else if (i2==num2) {q22 <- 100}

			for (i3 in 1:num3)
			{
				print(c(i1, i2, i3))
				q3 <- as.numeric(quantile(tab$pos[tab$group>=0], (i3-1)/num3))
				q33 <- as.numeric(quantile(tab$pos[tab$group>=0], i3/num3))

				if (i3==1) {q3 <- -100}
				else if (i3==num3) {q33 <- 100}
				
				tabred <- tab[((tab$group>=0) & (tab$sd>=q1) & (tab$sd<q11) & (tab$trend>=q2) & (tab$trend<q22) & (tab$pos>=q3) & (tab$pos<q33)),]
				groupid <- groupid + 1
				if (dim(tabred)[1] == 0)
				{
					restab <- rbind(restab, rbind(c(),c(groupid, q1, q11, q2, q22, q3, q33, 0)))
				}
				else
				{
					restab <- rbind(restab, rbind(c(),c(groupid, q1, q11, q2, q22, q3, q33, dim(tabred)[1])))
					tab$group[((tab$group>=0) & (tab$sd>=q1) & (tab$sd<q11) & (tab$trend>=q2) & (tab$trend<q22) & (tab$pos>=q3) & (tab$pos<q33))] <- groupid
				}	
				restab <- rbind(restab, c(groupid, q1, q11, q2, q22, q3, q33, dim(tabred)[1]))			
			}
		}
	}
	restab <- as.data.frame(restab)
	names(restab) <- c("groupid", "qsdstart", "qsdend", "qtrendstart", "qtrendend", "qposstart", "qposend", "quantity")
	list(tab=tab, restab=restab)
}

GetInfo <- function(tab)
{
	restab <- rbind(c(), c())
	groups <- unique(tab$group)
	groups <- groups[groups>=0]
	for (group in groups)
	{
		print(group)
		tabred <- tab[tab$group == group,]
		tempvec <- tabred$valfutur - tabred$valend
		restab <- rbind(restab, c(group, mean(tempvec), length(tempvec)))				
	}
	restab
}

Erase <- function(tab, mode="odd")
{
	days <- unique(tab$daystart)
	tab <- tab[tab$daystart == tab$dayend,]
	for (day in days)
	{
 		if (mode=="even")
		{
			if (day %% 2 == 0)
			{
				tab$group[((tab$daystart == day) & (tab$dayend == day))] <- -1
			}
		}
		else if (mode=="odd")
		{
			if (day %% 2 == 1)
			{
				tab$group[((tab$daystart == day) & (tab$dayend == day))] <- -1
			}
		}

	}	
tab
} 

EraseRandom <- function(tab, index)
{
	i <- 1
	days <- unique(tab$daystart)
	tab <- tab[tab$daystart == tab$dayend,]
	for (day in days)
	{
		if (index[i]==0)
		{
				tab$group[((tab$daystart == day) & (tab$dayend == day))] <- -1			
		}
		i <- i+1
	}
tab
}

FillsdGroups <- function(tab)
{
	sds <- c(0, 0.01, 0.03, 0.3, 1, Inf)
	groupnum <- 1
	for (j in 1:(length(sds) - 1))
	{
		tab$group[((tab$sd>=sds[j]) & (tab$sd<sds[j+1]) & (tab$group>=0))] <- groupnum
		groupnum <- groupnum + 1
	}
	tab
}

FilltrendsignGroups <- function(tab)
{
	groups <- floor(unique(tab$group))
	print(groups)
	groups <- groups[groups>0]
	for (group in groups)
	{
		print(group)
		tab$group[((tab$trend<0) & (tab$group==group))] <- group + 0.1
		tab$group[((tab$trend>=0) & (tab$group==group))] <- group + 0.9		 
	}
tab
}

FillposGroups <- function(tab)
{
	groups <- floor(unique(tab$group))
	print(groups)
	groups <- groups[groups>0]
	for (group in groups)
	{
		print(group)
		tab$group[((tab$pos< -0.008) & (tab$group==group))] <- group + 0.1
		tab$group[((tab$pos>=-0.008) & (tab$pos<0) & (tab$group==group))] <- group + 0.3		 
		tab$group[((tab$pos>=0) & (tab$pos<0.008) & (tab$group==group))] <- group + 0.7		 
		tab$group[((tab$pos>=0.008) & (tab$group==group))] <- group + 0.9		 
	}
tab

}

FillGroupInfo <- function(tab)
{
	restab <- rbind(c(), c())
	groups <- unique(tab$group[tab$group>0])
	for (group in groups)
	{
		tabred <- tab[tab$group==group,]

		if ((group>=10) & (group<20)) {q1 <- 0; q2 <- 0.01}
		else if ((group>=20) & (group<30)) {q1 <- 0.01; q2 <- 0.03}
		else if ((group>=30) & (group<40)) {q1 <- 0.03; q2 <- 0.3}
		else if ((group>=40) & (group<50)) {q1 <- 0.3; q2 <- 1}
		else {q1 <- 1; q2 <- 100}

		num <- floor(group %% 10)
		if (num==1) {t1 <- -100; t2 <- 0} else {t1 <- 0; t2 <- 100}

		num2 <- (group*10) %% 10

		if (num2==1) {p1 <- -100; p2 <- -0.008}		
		if (num2==3) {p1 <- -0.008; p2 <- 0}		
		if (num2==7) {p1 <- 0; p2 <- 0.008}		
		if (num2==9) {p1 <- 0.008; p2 <- 100}		


			quantity <- dim(tabred)[1]
		restab <- rbind(restab, rbind(c(), c(group, q1, q2, t1, t2, p1, p2, quantity)))
	}
	restab <- as.data.frame(restab)
	names(restab) <- c("groupid", "qsdstart", "qsdend", "qtrendstart", "qtrendend", "qposstart", "qposend", "quantity")
restab
}
