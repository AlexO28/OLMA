library(tseries)
library(MSBVAR)
library(vars)
###���������� � �������:
###������ ������� --- ���� � ������� R
###������ ������� --- �������� ���������

###�������, ���������� �������� ��� �����-�� ���������� ���������
###����: �������� ������, �� ������� ������� ������ ��� ����� � ��� ��������
###val --- ������
###shift --- ������ ����
###�����: ������� �� ���������:
###����� �������, ������� �� ������, p-value ����� ��������� �� ������ (������� �������, ����� �������), ����������� ����� �����
CausalityStrats <- function(str1, str2, val, shift)
{
	tab1 <- read.csv(str1,header=FALSE)
	tab2 <- read.csv(str2,header=FALSE)
	print("TOTAL DATA")
	print(dim(tab1))
	print(dim(tab2))
	tab1 <- PrepareData1(tab1)
	tab2 <- PrepareData1(tab2)

	restab <- rbind(c())

	shiftval <- 24*60*60
	cutval <- 30*60
	print(shiftval)
	i0 <- 0; i1 <- shiftval
	counter <- 1
	maxdata <- min(tab1[dim(tab1)[1],1], tab2[dim(tab2)[1],1])

	while (i0<maxdata)
	{
		tab1red <- data.frame(v1=c(), v2=c())
		tab2red <- tab1red
		#tab1redfull <- data.frame(v1=c(), v2=c())
		#tab2redfull <- tab1red
		tab1redfull <- tab1[(tab1[,1]>=i0) & (tab1[,1]<=i1),]
		tab2redfull <- tab2[(tab2[,1]>=i0) & (tab2[,1]<=i1),]
		datemin <- max(tab1redfull[1,1], tab2redfull[1,1])
		datemax <- min(tab1redfull[dim(tab1redfull)[1],1], tab2redfull[dim(tab2redfull)[1],1])
		tab1redfull <- tab1redfull[(tab1redfull[,1] >= datemin + cutval) & (tab1redfull[,1] <=  datemax - cutval),]
		tab2redfull <- tab2redfull[(tab2redfull[,1] >= datemin + cutval) & (tab2redfull[,1] <=  datemax - cutval),]
		if (dim(tab1redfull)[1] < 100) 
		{
			i0 <- i1
			i1 <- i1 + shiftval
			if (i0 > maxdata) {break}
			next
		}
		datemin <- max(tab1redfull[1,1], tab2redfull[1,1])
		datemax <- min(tab1redfull[dim(tab1redfull)[1],1], tab2redfull[dim(tab2redfull)[1],1])
		winstart <- datemin
		winend <- winstart + round(shiftval * shift) 
		
		while (winstart < datemax)
		{
			tab1red <- tab1redfull[(tab1redfull[,1] >= winstart) & (tab1redfull[,1]<=winend),]
			tab2red <- tab2redfull[(tab2redfull[,1] >= winstart) & (tab2redfull[,1]<=winend),]
			if (dim(tab1red)[1] < 200)
			{
				winstart <- winend
				winend <- winend + round(shiftval*shift)
				next		
			}

			hh<-ParseData(tab1red, tab2red, val)

			names(hh$tab1) <- c("v1","v2")
			names(hh$tab2) <- c("v1","v2")
			
			#scale1 <- 1000
			#scale2 <- 1
			scale1 <- 1
			scale2 <- 1000
			hh$tab1[,2] <- hh$tab1[,2]/scale1
			hh$tab2[,2] <- hh$tab2[,2]/scale2

			tab1rednew <- PrepareData2(hh$tab1)
			tab2rednew <- PrepareData2(hh$tab2)

			tab1rednew <- tab1rednew[2:(dim(tab1rednew)[1] - 1),]
			tab2rednew <- tab2rednew[2:(dim(tab2rednew)[1] - 1),]
			tab1red <- tab1rednew
			tab2red <- tab2rednew

			if (dim(tab1red)[1] < 200)
			{
				winstart <- winend
				winend <- winend + round(shiftval*shift)
				next		
			}


			hh <- cbind(tab1red[,2], tab2red[,2])
			mm <- VAR(hh, lag.max=50)
			lags <- mm$p
			temp <- granger.test(hh, p=lags)

			modres <- CausalityStrategy(tab1red, tab2red, jump1, jump2, jump3, jump4)

			print(paste("result", modres))

			restab <- rbind(restab, c(counter,modres,temp[1,2],temp[2,2],lags))

			print(counter)

			winstart <- winend
			winend <- winend + round(shiftval * shift)
			counter <- counter + 1
		}
		i0 <- i1
		i1 <- i1 + shiftval
	}
restab
}

###���������� ���������
###����: ��� ����-������, ��������������� ����� � �������� �� ������ ������
###��������� ������:
###jump1 --- ������ ��������
###jump3 --- ��������� ������ �����
###�����: ������� �� ����

source("Strategies.r") ###��� ������� Buy, Sell

CausalityStrategy <- function(tab1, tab2, jump1, jump2, jump3, jump4)
{
	stoploss <- -Inf

	ctab1 <- cumsum(tab1[,2])
	ctab2 <- cumsum(tab2[,2])
	upper <- 0
	lower <- 0
	cash <- 0
	assets <- 0
	###rent <- 0.03 ####this is for the dollar
	rent <- 0.03
	print(paste("rent",rent))
	prevval <- 0
	prevj <- 0

	###for (j in 1:dim(tab1)[1])
	j <- 1
	while (j < dim(tab1)[1])
	{
		#####print(j)

		if (cash + Normalize(cash, assets, ctab1[j]-rent, ctab1[j]+rent)< stoploss) {break}

		if (assets>0)
		{
			if (ctab1[j] - prevval > jump3)
			{
				temp <- Sell(ctab1[j]-rent,1)
				assets <- assets - 1
				cash <- cash + temp[1]			
				print(paste("diff is",j-prevj))
			}
			j <- j+1
		}
		else if (assets<0)
		{
			if (ctab1[j] - prevval < -jump3)
			{
				temp <- Buy(ctab1[j]+rent,1)
				assets <- assets + 1
				cash <- cash + temp[1]			
				print(paste("diff is",j-prevj))
			}
			j <- j+1			
		}
		else
		{
			if (abs(tab2[j,2])>jump1)
			{
			###�������� ���������
				if ((tab2[j,2]>0) & (tab1[j,2]<jump2))
				{	
					j <- j + 1
				
					###�� ������, ��� ����� �������� ����
					temp <- Buy(ctab1[j]+rent,1)
					assets <- assets + 1
					cash <- cash + temp[1]			
					prevval <- ctab1[j]
					prevj <- j

					j <- j+1
				}			
				else if ((tab2[j,2]<0) & (tab1[j,2]> -jump2))
				{
					j <- j + 1

					###�� ������, ��� ����� ����� ����
					temp <- Sell(ctab1[j]-rent,1)
					assets <- assets - 1
					cash <- cash + temp[1]			
					prevval <- ctab1[j]
					prevj <- j
	
					j <- j+1
				}
				else {j <- j + 1}
			}
			else {j <- j+1}
		}

	}
cash <- cash + Normalize(cash, assets, ctab1[j]-rent, ctab1[j]+rent)
#print(cash)
cash
}

###������������� 1-� ������� ����, 2-� ������� ����, 3 -� ������� bid, 4-� ������� ask
CausalityStratsMod <- function(str1, str2, val, shift)
{
	tab1 <- read.csv(str1,header=FALSE,sep=" ")
	tab2 <- read.csv(str2,header=FALSE,sep=" ")
	print("TOTAL DATA")
	print(dim(tab1))
	print(dim(tab2))
	tab1 <- PrepareData1mod(tab1)
	tab2 <- PrepareData1mod(tab2)

	print(summary(tab1))
	print(summary(tab2))

	restab <- rbind(c())

	shiftval <- 24*60*60
	cutval <- 30*60
	print(shiftval)
	i0 <- 0; i1 <- shiftval
	counter <- 1
	maxdata <- min(tab1[dim(tab1)[1],1], tab2[dim(tab2)[1],1])

	while (i0<maxdata)
	{
		tab1red <- data.frame(v1=c(), v2=c())
		tab2red <- tab1red
		tab1redfull <- tab1[(tab1[,1]>=i0) & (tab1[,1]<=i1),]
		tab2redfull <- tab2[(tab2[,1]>=i0) & (tab2[,1]<=i1),]

		datemin <- max(tab1redfull[1,1], tab2redfull[1,1])
		datemax <- min(tab1redfull[dim(tab1redfull)[1],1], tab2redfull[dim(tab2redfull)[1],1])
		tab1redfull <- tab1redfull[(tab1redfull[,1] >= datemin + cutval) & (tab1redfull[,1] <=  datemax - cutval),]
		tab2redfull <- tab2redfull[(tab2redfull[,1] >= datemin + cutval) & (tab2redfull[,1] <=  datemax - cutval),]
		if (dim(tab1redfull)[1] < 100) 
		{
			i0 <- i1
			i1 <- i1 + shiftval
			if (i0 > maxdata) {break}
			next
		}
		datemin <- max(tab1redfull[1,1], tab2redfull[1,1])
		datemax <- min(tab1redfull[dim(tab1redfull)[1],1], tab2redfull[dim(tab2redfull)[1],1])
		winstart <- datemin
		winend <- winstart + round(shiftval * shift) 
		
		while (winstart < datemax)
		{
			tab1red <- tab1redfull[(tab1redfull[,1] >= winstart) & (tab1redfull[,1]<=winend),]
			tab2red <- tab2redfull[(tab2redfull[,1] >= winstart) & (tab2redfull[,1]<=winend),]
			if (dim(tab1red)[1] < 200)
			{
				winstart <- winend
				winend <- winend + round(shiftval*shift)
				next		
			}

			hh<-ParseDatamod(tab1red, tab2red, val)

			names(hh$tab1) <- c("v1","v2","v3","v4")
			names(hh$tab2) <- c("v1","v2","v3","v4")
			
			#scale1 <- 1000
			#scale2 <- 1
			scale1 <- 1
			scale2 <- 1000
			hh$tab1[,2] <- hh$tab1[,2]/scale1
			hh$tab2[,2] <- hh$tab2[,2]/scale2

			tab1rednew <- PrepareData2(hh$tab1)
			tab2rednew <- PrepareData2(hh$tab2)
			tab1rednew <- tab1rednew[2:(dim(tab1rednew)[1] - 1),]
			tab2rednew <- tab2rednew[2:(dim(tab2rednew)[1] - 1),]
			tab1red <- tab1rednew
			tab2red <- tab2rednew

			if (dim(tab1red)[1] < 200)
			{
				winstart <- winend
				winend <- winend + round(shiftval*shift)
				next		
			}


			hh <- cbind(tab1red[,2], tab2red[,2])
			mm <- VAR(hh, lag.max=50)
			lags <- mm$p
			temp <- granger.test(hh, p=lags)

			modres <- CausalityStrategyMod(tab1red, tab2red, jump1, jump2, jump3, jump4)
			print(modres)
			restab <- rbind(restab, c(counter,modres,temp[1,2],temp[2,2],lags))

			print(counter)

			winstart <- winend
			winend <- winend + round(shiftval * shift)
			counter <- counter + 1
		}
		i0 <- i1
		i1 <- i1 + shiftval
	}
restab
}

###���������� ���������
###����: ��� ����-������, ��������������� ����� � �������� �� ������ ������
###��������� ������:
###jump1 --- ������ ��������
###jump3 --- ��������� ������ �����
###�����: ������� �� ����
CausalityStrategyMod <- function(tab1, tab2, jump1, jump2, jump3, jump4)
{
	stoploss <- -Inf

	ctab1 <- cumsum(tab1[,2])
	ctab2 <- cumsum(tab2[,2])
	upper <- 0
	lower <- 0
	cash <- 0
	assets <- 0
	###rent <- 0.03 ####this is for the dollar
	rent <- 0.02
	print(paste("rent",rent))
	prevval <- 0
	prevj <- 0

	###for (j in 1:dim(tab1)[1])
	j <- 1
	while (j < dim(tab1)[1])
	{
		#####print(j)

		if (cash + Normalize(cash, assets, tab1[j,3]-rent, tab1[j,4]+rent)< stoploss) {break}

		if (assets>0)
		{
			if (ctab1[j] - prevval > jump3)
			{
				temp <- Sell(tab1[j,3]-rent,1)
				assets <- assets - 1
				cash <- cash + temp[1]			
				print(paste("diff is",j-prevj))
			}
			j <- j+1
		}
		else if (assets<0)
		{
			if (ctab1[j] - prevval < -jump3)
			{
				temp <- Buy(tab1[j,4]+rent,1)
				assets <- assets + 1
				cash <- cash + temp[1]			
				print(paste("diff is",j-prevj))
			}
			j <- j+1			
		}
		else
		{
			if (abs(tab2[j,2])>jump1)
			{
			###�������� ���������
				if ((tab2[j,2]>0) & (tab1[j,2]<jump2))
				{	
					###j <- j + 1
				
					###�� ������, ��� ����� �������� ����
					temp <- Buy(tab1[j,4]+rent,1)
					assets <- assets + 1
					cash <- cash + temp[1]			
					prevval <- ctab1[j]
					prevj <- j

					j <- j+1
				}			
				else if ((tab2[j,2]<0) & (tab1[j,2]> -jump2))
				{
					###j <- j + 1

					###�� ������, ��� ����� ����� ����
					temp <- Sell(tab1[j,3]-rent,1)
					assets <- assets - 1
					cash <- cash + temp[1]			
					prevval <- ctab1[j]
					prevj <- j
	
					j <- j+1
				}
				else {j <- j + 1}
			}
			else {j <- j+1}
		}

	}
cash <- cash + Normalize(cash, assets, tab1[j,3]-rent, tab1[j,4]+rent)
#print(cash)
cash
}

###�������, �������� ���������� VAR-������ �� ������� ������� � ������� ����
###����: �������� ������ ��� ����� � ��������
###����, ������, ������ ����
###�����: ������
###$vec --- �������� ������
###$kefs --- ���� ������
###$tab1 --- �������� �����
###$tab2 --- �������� ��������
###$res1, $res2 --- ��������� dynamictestingseconds
VarTest <- function(str1,str2, val, shift, day)
{
	res1 <- DynamicTestingSecondsInside(str1, str2, shift, val, 0, day, c(0,0,0))
	tab1 <- res1$tab1; tab2 <- res1$tab2
	res2 <- DynamicTestingSecondsInside(str1, str2, shift, val, 0, day-1, c(0,0,0))
	tab1prev <- res2$tab1; tab2prev <- res2$tab2

	print(dim(tab1))
	print(dim(tab1prev))


	hhh <- cbind(tab1prev[,2], tab2prev[,2])
	mmm <- VAR(hhh, lag.max=50)
	print(causality(mmm))
	kefs <- as.numeric(coef(mmm)$y1[,1])
	print(kefs)
	print(summary(tab1))
	print(summary(tab1prev))
	vec <- MyPredict(tab1prev, tab2prev, kefs)
	list(vec=vec, kefs=kefs, tab1=tab1, tab2=tab2, res1=res1, res2=res2, tab1prev=tab1prev, tab2prev=tab2prev)
}

###������� ������������ �������� �� VAR-������
###����: tab1 --- ������ �� �����, tab2 --- ������ �� ��������
###coefs --- ������������ VAR-������.
###� ������ ������ ������� ����������� �� VAR-������ �����, ������
###�� ���������� �������� ����� � ��������
###�����: ������, ���������� �������� ����� �� VAR-������
MyPredict <- function(tab1, tab2, coefs)
{
	len <- floor(length(coefs)/2)
	print(len)
	vec <- tab1[1:len,2]
	for (j in (len + 1):dim(tab1)[1])
	{
		summa <- coefs[2*len+1]
		k <- 1
		while(k+1<=2*len)
		{
		#	print("***")
		#	print(k)
		#	print(summa)
		#	print(c(coefs[k], coefs[k+1]))
		#	print(c(tab1[j - (k+1)/2,2]))
		#	print(c(tab2[j - (k+1)/2,2]))

			summa <- summa + coefs[k]*tab1[j - (k+1)/2,2] + coefs[k+1]*tab2[j - (k+1)/2,2]	
			k <- k+2
		}	
	#####	break
		vec <- c(vec, summa)
	}
	vec
}
###Not run
Test1 <- function(tab1, tab2)
{
###tab1 is a stock
###tab2 is a futures
totalcount <- 0
wincount <- 0
sign2 <- 0
sign1 <- 0
for (j in 1:dim(tab2)[1])
{
	if ((tab2[j,2]!=0) & (tab1[j,2]*tab2[j,2]<=0))
	{
		sign2 <- sign(tab2[j,2])
		if (sign2==0) {print("ERROR!")}
		for (k in ((j+1):dim(tab1)[1]))
		{
			if (tab1[k,2]!=0) 
			{
				totalcount <- totalcount + 1
				sign1 <- sign(tab1[k,2])
				if (sign1 == 0) {print("ERROR")}				
				if (sign2 == sign1)
				{
					wincount <- wincount + 1
				}
				break
			}			
		}
	}
}
c(wincount, totalcount)
}
###not run
Test2 <- function(tab1, tab2)
{
###tab1 is a stock
###tab2 is a futures
totalcount <- 0
wincount <- 0
sign2 <- 0
sign1 <- 0
for (j in 1:dim(tab2)[1])
{
	if (tab2[j,2]!=0)
	{
		totalcount <- totalcount + 1
		if (sign(tab1[j+1,2]) == sign(tab2[j,2]))
		{
			wincount <- wincount + 1
		}
	}
}
c(wincount, totalcount)
}
###not run
Test3 <- function(tab1, tab2)
{
###tab1 is a stock
###tab2 is a futures
totalcount <- 0
wincount <- 0
sign2 <- 0
sign1 <- 0
for (j in 1:dim(tab2)[1])
{
	if ((tab2[j,2]!=0) & (tab1[j,2]*tab2[j,2]<0))
	{
		totalcount <- totalcount + 1
		if (sign(tab1[j+1,2]) == sign(tab2[j,2]))
		{
			wincount <- wincount + 1
		}
	}
}
c(wincount, totalcount)
}
###not run
Test4 <- function(tab1, tab2)
{
###tab1 is a stock
###tab2 is a futures

totalcount <- 0
wincount <- 0
sign2 <- 0
sign1 <- 0
for (j in 1:dim(tab2)[1])
{
	if ((tab2[j,2]!=0) & (tab1[j,2]*tab2[j,2]<0))
	{
		sign2 <- sign(tab2[j,2])
		if (sign2==0) {print("ERROR!")}
		if (j == dim(tab1)[1]) {break}
		for (k in ((j+1):dim(tab1)[1]))
		{
			if (tab1[k,2]!=0) 
			{
				totalcount <- totalcount + 1
				sign1 <- sign(tab1[k,2])
				if (sign1 == 0) {print("ERROR")}				
				if (sign2 == sign1)
				{
					wincount <- wincount + 1
				}
				break
			}			
		}
	}
}
c(wincount, totalcount)
}
###����
###���� --- tab1, �����, tab2, �������, criticalvals --- ������ �� 3 �����
###����������� ���������:
###���� � �����-�� ������ ��������� ������ �������� (�����) �� ����� ��� �� criticalvals[1]
###� ��������� ������ ����� �� ����� ��� �� criticalvals[2] (����)
###�� �� ��� ������������� ������ ����� (�����) �� ����� ��� �� criticalvals[3]
###�� ����������� � ��������� ������
###���� �� ������� ����
###�������� ������ ����� (����), ������� ������ �������� (����) 
###�����: ������� �� ���������
###����� �������, ����� ���������
Test5 <- function(tab1, tab2, criticalvals)
{
totalcount <- 0
wincount <- 0
sign2 <- 0
criticalval1 <- criticalvals[1]
criticalval2 <- criticalvals[2]
criticalval3 <- criticalvals[3]
for (j in 1:dim(tab2)[1])
{
	sign2 <- sign(tab2[j,2])
	val <- abs(tab2[j,2])
	val2 <- abs(tab1[j,2])
	if ((val>=criticalval1) & (sign2!=0) & (sign2*tab1[j,2]<0) & (val2>=criticalval2))
	{
		if (j == dim(tab1)[1]) {break}
		valnew <- 0
		for (k in ((j+1):dim(tab1)[1]))
		{
			valnew <- valnew + abs(tab1[k,2])
			if ((sign(tab1[k,2])*sign2 > 0) & (valnew>=criticalval3))
			{
				totalcount <- totalcount + 1
				wincount <- wincount + 1
				break
			}
			else
			if (sign(tab1[k,2])*sign2 < 0) 
			{
				totalcount <- totalcount + 1
				break
			}
			else
			if (sign(tab2[k,2])*sign2 < 0)
			{
				totalcount <- totalcount + 1
				break
			}			
		}

	}
}
c(wincount, totalcount)
}
###������� ������ � �������
###tab1 --- �����, tab2 --- �������
###val --- ������
###������� ������ ��� ����� �� ������� ����� val
###� ����� ����� �� ���������� ����� � �������� � ��� �������
###�����: ���������� �������� ����� � ��������
ParseData <- function(tab1, tab2, val=20)
{
	datemin <- max(tab1[1,1], tab2[1,1])
	datemax <- min(tab1[dim(tab1)[1],1], tab2[dim(tab2)[1],1])
	num <- ((datemax - datemin) %/% val)

	vec <- datemin + val*(0:num)
	j <- 1; k <- 1
	val1 <- c()
	val2 <- c()


	for (i in 1:length(vec))
	{
		while (1==1)
		{
			if (j>dim(tab1)[1]) {j <- dim(tab1)[1]; break}
			if (tab1[j,1] > vec[i])
			{
				j <- j-1
				break
			}
			else {j <- j+1}
		}	
		while (1==1)
		{
			if (k>dim(tab2)[1]) {k <- dim(tab2)[1]; break}
			if (tab2[k,1] > vec[i])
			{
				k <- k-1
				break
			}
			else {k <- k+1}
		}	
		val1 <- c(val1, tab1[j,2])
		val2 <- c(val2, tab2[k,2])
	}
list(tab1=data.frame(v1=vec, v2=val1), tab2=data.frame(v1=vec, v2=val2))
}
ParseDatamod <- function(tab1, tab2, val=20)
{
	datemin <- max(tab1[1,1], tab2[1,1])
	datemax <- min(tab1[dim(tab1)[1],1], tab2[dim(tab2)[1],1])
	num <- ((datemax - datemin) %/% val)

	vec <- datemin + val*(0:num)
	j <- 1; k <- 1
	val1 <- c()
	val2 <- c()
	valbid1 <- c()
	valbid2 <- c()
	valask1 <- c()
	valask2 <- c()

	for (i in 1:length(vec))
	{
		while (1==1)
		{
			if (j>dim(tab1)[1]) {j <- dim(tab1)[1]; break}
			if (tab1[j,1] > vec[i])
			{
				j <- j-1
				break
			}
			else {j <- j+1}
		}	
		while (1==1)
		{
			if (k>dim(tab2)[1]) {k <- dim(tab2)[1]; break}
			if (tab2[k,1] > vec[i])
			{
				k <- k-1
				break
			}
			else {k <- k+1}
		}	
		val1 <- c(val1, tab1[j,2])
		val2 <- c(val2, tab2[k,2])
		valbid1 <- c(valbid1, tab1[j,3])
		valask1 <- c(valask1, tab1[j,4])
		valbid2 <- c(valbid2, tab2[k,3])
		valask2 <- c(valask2, tab2[k,4])
	}
list(tab1=data.frame(v1=vec, v2=val1, v3=valbid1, v4=valask1), tab2=data.frame(v1=vec, v2=val2, v3=valbid2, v4=valask2))
}
###������� ������������
###����:
###tabred1 --- �����, tabred2 --- �������
###winvec, totalvec --- ������������ ������� ��� ����� 5
###counter --- ����� �������
###criticalvals
###�����: ������
###������:$res
###����� �������,
###���������� ������ � �������
###��������� 6 �������� --- ���������� ��������� ������ �� ��������������
###��������� 4 ������� --- ���������� ������ �� ����������� �� ����������
###����� ����� ������������ � ������� VAR (�� ���������)
###�������� ������ 0.05 �������, ��� ������� �������� �� ���������� ��������� ����� ���������
###����� �����
###��������� 4 ������� --- ������ ������������ VAR-������
###�������: $winvec, $totalvec --- ����������� � ����������� ����� 5 
Checker <- function(tab1red, tab2red, winvec, totalvec, counter, criticalvals=c(0,0,0))
{
#	print("***")
#	print(dim(tab1red))
#	print(dim(tab2red))
#	print(summary(tab1red))
#	print(summary(tab2red))
	

	dimnum <- dim(tab1red)[1]
	temp <- Box.test(tab1red[,2],10,"Ljung-Box")
	bt1num <- temp$p.value
	temp <- Box.test(tab2red[,2],10,"Ljung-Box")
	bt2num <- temp$p.value
	temp <- adf.test(tab1red[,2], alternative="stationary")
	adf1num <- temp$p.value		
	temp <- adf.test(tab2red[,2], alternative="stationary")
	adf2num <- temp$p.value		
	temp <- kpss.test(tab1red[,2])
	kpss1num <- temp$p.value
	temp <- kpss.test(tab2red[,2])
	kpss2num <- temp$p.value
	hh <- cbind(tab1red[,2], tab2red[,2])

	lagnum <- 0
	model <- VAR(hh,lag.max=50)
	lagnum <- max(lagnum, model$p)
	temp <- causality(model)
	val1 <- model$varresult$y1$coefficients[1]
	val2 <- model$varresult$y1$coefficients[2]
	val3 <- model$varresult$y2$coefficients[1]
	val4 <- model$varresult$y2$coefficients[2]
	cas1num <- temp$Granger$p.value
	temp <- granger.test(hh, p=lagnum)
	gtnum1 <- temp[1,2]
	gtnum2 <- temp[2,2]

	hh <- cbind(tab2red[,2], tab1red[,2])
	model <- VAR(hh,p=lagnum)
	temp <- causality(model)
	cas2num <- temp$Granger$p.value

		tempvec <- c(0,0)
	tempvec <- Test5(tab1red, tab2red, criticalvals)
	winvec <- c(winvec, tempvec[1])
	totalvec <- c(totalvec, tempvec[2])
		
#		print(c(counter, dimnum, bt1num, bt2num, adf1num, adf2num, kpss1num,
#				kpss2num, gtnum, cas1num, cas2num))

###������� ������� ������ �����
###����� ����� ������ �������
###����� ����� ������ �������
###� ������� ������� ������ �����

list(res=c(counter, dimnum, bt1num, bt2num, adf1num, adf2num, kpss1num,
				kpss2num, gtnum1, gtnum2, cas1num, cas2num, lagnum, val1, val2, val3, val4),
	winvec=winvec, totalvec=totalvec)
}
###not run
Tester <- function(str, vec=c(0, 0.0002, 0.0004, 0.0006, 0.0008, 0.001))
{
	for (check1 in vec)
	{
	for (check2 in vec)
	{
	for (check3 in vec)
	{
		if (check3>max(check1,check2)) {next}
		criticalvals <- c(check1, check2, check3)
		res <- DynamicTestingSecondsInside("s7s.txt","s7f.txt", 1/80, 1, skip=runif(1,0,60), criticalvals=criticalvals)
		tvec <- res$win/res$total
		tnum <- length(tvec[is.na(tvec)==TRUE])/length(tvec)
		tvec <- tvec[is.na(tvec)==FALSE]
		num1 <- mean(tvec)
		num2 <- median(tvec)
		write.table(rbind(c(),c(criticalvals, tnum, num1, num2)), str, append=TRUE, row.names=FALSE, col.names=FALSE)
	}
	}
	}
}
###������� ������ � ������������� �������
###����:
###str1 --- ����, �� �������� ������� ������� ������ ������
###str2 --- ����, �� �������� ������� ������� ������ ��������
###�������� � ������: ������ ���� ������������� �� ����� 1 �������� ��������� (���� ��������� ������)
###shift --- ������ ���� (1 --- ��� 1 ����, 1/6 --- 4 ���� � �.�.)
###val --- ������ � ��������
###skip --- ���������� �������-�� ������
###breakpoint --- ���������������, ��������� ���� ������
###criticalvals --- ���� ��� Test5
###�����:
###������:
###$res --- ������� ����������� ������������, �� ��� �������� Checker � ��������� ������
###����� ��� ������� ����� ������� � ������� Analysis
###$tab1 --- �������� ����� �� ��������� ������
###$tab2 --- �������� �������� �� ��������� ������
###$save1, $save2
###$winvec --- ����� �������� ����������� ����������� ����� 5
###$totalvec --- ����� ����� ����������� ����������� ����� 5
###� ���� ������� ��������������� ������� ���������� x(t+1) - x(t)
DynamicTestingSecondsInside <- function(str1="qq1.txt", str2="qq2.txt", shift=1, val=20, skip=0, breakpoint=Inf, criticalvals=c(0,0,0))
{
	print("new")
	totalvec <- c(); winvec <- c()

	###tick data
	tab1 <- read.csv(str1,header=FALSE)
	tab2 <- read.csv(str2,header=FALSE)
	print("TOTAL DATA")
	print(dim(tab1))
	print(dim(tab2))
	print(tab1[1,])
	print(tab2[1,])

	tab1 <- PrepareData1(tab1)
	tab2 <- PrepareData1(tab2)

	print(summary(tab1))
	print(summary(tab2))

	restab <- data.frame(count=c(), dim=c(), bt1=c(), bt2=c(), 
			adf1=c(), adf2=c(), kpss1=c(), kpss2=c(), gt1=c(), gt2=c(), cas1=c(), cas2=c())
	shiftval <- 24*60*60
	cutval <- 30*60
	i0 <- 0; i1 <- shiftval
	counter <- 1
	counterid <- 1
		helper <- 5
		period <- 1
	maxdata <- min(tab1[dim(tab1)[1],1], tab2[dim(tab2)[1],1])
	print(paste("maxdata", maxdata))
	print(tab1[dim(tab1)[1],1])
	print(tab2[dim(tab2)[1],1])
	while (i0<maxdata)
	{
		print(paste(c(i0, i1),"!!!!"))		
		if (counterid>=breakpoint) {print("full stop"); break}

		tab1red <- data.frame(v1=c(), v2=c())
		tab2red <- tab1red
		tab1redfull <- data.frame(v1=c(), v2=c())
		tab2redfull <- tab1red
		tab1redfull <- tab1[(tab1[,1]>=i0) & (tab1[,1]<=i1),]
		tab2redfull <- tab2[(tab2[,1]>=i0) & (tab2[,1]<=i1),]
		datemin <- max(tab1redfull[1,1], tab2redfull[1,1])
		datemax <- min(tab1redfull[dim(tab1redfull)[1],1], tab2redfull[dim(tab2redfull)[1],1])
		tab1redfull <- tab1redfull[(tab1redfull[,1] >= datemin + cutval + skip) & (tab1redfull[,1] <=  datemax - cutval),]
		tab2redfull <- tab2redfull[(tab2redfull[,1] >= datemin + cutval + skip) & (tab2redfull[,1] <=  datemax - cutval),]
		
		if (dim(tab1redfull[is.na(tab1redfull[,1])==FALSE,])[1] < 100) 
		{
			print("skipping")
			i0 <- i1
			i1 <- i1 + shiftval
			if (i0 > maxdata) {break}
			next
		}
		datemin <- max(tab1redfull[1,1], tab2redfull[1,1])
		datemax <- min(tab1redfull[dim(tab1redfull)[1],1], tab2redfull[dim(tab2redfull)[1],1])
		winstart <- datemin
		winend <- winstart + round(shiftval * shift) 

		save1 <- c()
		save2 <- c()

		while (winstart<datemax)
		{
			print("in")
			print(c(winstart, winend))

			tab1red <- tab1redfull[(tab1redfull[,1] >= winstart) & (tab1redfull[,1]<=winend),]
			tab2red <- tab2redfull[(tab2redfull[,1] >= winstart) & (tab2redfull[,1]<=winend),]
			if (dim(tab1red[is.na(tab1red[,1])==FALSE,])[1] < 50)
			{
				print("Not enough data!")
				winstart <- winend
				winend <- winend + round(shiftval*shift)
				next		
			}


			hh<-ParseData(tab1red, tab2red, val)
			print("parseend")
			names(hh$tab1) <- c("v1","v2")
			names(hh$tab2) <- c("v1","v2")
			tab1red <- hh$tab1
			tab2red <- hh$tab2

#			save1 <- hh$tab1
#			save2 <- hh$tab2
			tab1rednew <- PrepareData2(tab1red)
			
			tab2red[,2] <- tab2red[,2]/1000

			tab2rednew <- PrepareData2(tab2red)
			tab1rednew <- tab1rednew[2:(dim(tab1rednew)[1] - 1),]
			tab2rednew <- tab2rednew[2:(dim(tab2rednew)[1] - 1),]
			tab1red <- tab1rednew
			tab2red <- tab2rednew

			if (dim(tab1red)[1] < 200)
			{
#				print("Not enough data")
#				print(dim(tab1red))
				winstart <- winend
				winend <- winend + round(shiftval*shift)
				next		
			}

	#		tab1red[,2] <- log(tab1red[,2])
	#		tab2red[,2] <- log(tab2red[,2])

			temp <- Checker(tab1red, tab2red, winvec, totalvec, counter, criticalvals)
			restab <- rbind(restab, temp$res)

#			print("***")
#			print(counterid)
#			print(dim(tab1red))
#			print(dim(restab))


			winvec <- temp$winvec
			totalvec <- temp$totalvec

			winstart <- winend
			winend <- winend + round(shiftval * shift)

			if (counterid>=breakpoint) {print("FULLSTOP");break}
			counterid <- counterid + 1

			save1 <- tab1red
			save2 <- tab2red

		}
		i0 <- i1
		i1 <- i1 + shiftval

		counter <- counter + 1

	}
list(res=restab, tab1=tab1red, tab2=tab2red, save1=save1, save2=save2, win=winvec, total=totalvec)
}

###������� ������ � ������ ���������, �� �� �� ������������� ������
###����:
###str1 --- ����, �� �������� ������� ������� ������ ������
###str2 --- ����, �� �������� ������� ������� ������ ��������
###shift --- ������ ���� (1 --- ��� 1 ����, 2 --- ��� 2 ��� � �.�.)
###val --- ������ � ��������
###skip --- ���������� �������-�� ������
###breakpoint --- ���������������, ��������� ���� ������
###�����:
###������:
###$res --- ������� ����������� ������������, �� ��� �������� Checker � ��������� ������
###����� ��� ������� ����� ������� � ������� Analysis
###$tab1 --- �������� ����� �� ��������� ������
###$tab2 --- �������� �������� �� ��������� ������
###$save1, $save2
###$winvec, $totalvec
###�� ��������� �� ������� PREPAREDATA2 ��� �� ����� ���������
DynamicTestingSeconds <- function(str1="qq1.txt", str2="qq2.txt", shift=1, val=20, skip=0, breakpoint=Inf)
{
	totalvec <- c(); winvec <- c()

	###tick data
	tab1 <- read.csv(str1,header=FALSE)
	tab2 <- read.csv(str2,header=FALSE)
	print("TOTAL DATA")
	print(dim(tab1))
	print(dim(tab2))
	tab1 <- PrepareData1(tab1)
	tab2 <- PrepareData1(tab2)
	restab <- data.frame(count=c(), dim=c(), bt1=c(), bt2=c(), 
			adf1=c(), adf2=c(), kpss1=c(), kpss2=c(), gt1=c(), gt2=c(), cas1=c(), cas2=c())
	shiftval <- 24*60*60
	cutval <- 30*60
	print(shiftval)
	i0 <- 0; i1 <- shiftval
	counter <- 1
		helper <- 5
		period <- 1
	maxdata <- min(tab1[dim(tab1)[1],1], tab2[dim(tab2)[1],1])
#	print("maxdata")
#	print(maxdata)
	while (i0<maxdata)
	{
		if (counter>breakpoint) {break}
		tab1red <- data.frame(v1=c(), v2=c())
		tab2red <- tab1red
		j <- 1
		while (j<=shift)
		{
			print("in")
			helper <- helper + 1
			if (helper>7) {helper <- 1; period <- period + 1}
			tab1rednew <- tab1[(tab1[,1]>=i0) & (tab1[,1]<=i1),]
			tab2rednew <- tab2[(tab2[,1]>=i0) & (tab2[,1]<=i1),]
			datemin <- max(tab1rednew[1,1], tab2rednew[1,1])
			datemax <- min(tab1rednew[dim(tab1rednew)[1],1], tab2rednew[dim(tab2rednew)[1],1])
			tab1rednew <- tab1rednew[(tab1rednew[,1] >= datemin+cutval+skip) & (tab1rednew[,1] <= datemax-cutval),]
			tab2rednew <- tab2rednew[(tab2rednew[,1] >= datemin+cutval+skip) & (tab2rednew[,1] <= datemax-cutval),]

			if (dim(tab1rednew[is.na(tab1rednew[,1])==FALSE,])[1] < 100) 
			{
				print("skipping")
				i0 <- i1
				i1 <- i1 + shiftval
				if (i0 > maxdata) {break}
				next
			}

#			print("cutdata")
#			print(dim(tab1rednew))
#			print(dim(tab2rednew))
			hh<-ParseData(tab1rednew, tab2rednew, val)
			names(hh$tab1) <- c("v1","v2")
			names(hh$tab2) <- c("v1","v2")
			save1 <- hh$tab1
			save2 <- hh$tab2
			tab1rednew <- PrepareData2(hh$tab1)
			tab2rednew <- PrepareData2(hh$tab2)
			tab1rednew <- tab1rednew[2:(dim(tab1rednew)[1] - 1),]
			tab2rednew <- tab2rednew[2:(dim(tab2rednew)[1] - 1),]
			tab1red <- rbind(tab1red, tab1rednew)
			tab2red <- rbind(tab2red, tab2rednew)

			print("dimensions of tables")
			print(dim(tab1red))
			print(dim(tab2red))

			i0 <- i1
			i1 <- i1 + shiftval
			j <- j + 1
		}
		if ((dim(tab1red)[1] <= 100) | (dim(tab2red)[1]<=100)) 
		{
			i0 <- i1
			i1 <- i1 + shiftval
			next
		}
		tab1red[,2] <- log(tab1red[,2])
		tab2red[,2] <- log(tab2red[,2])

#print(summary(tab1red))
#print(summary(tab2red))

#		print("CYCLE ENDED")
#		print(dim(tab1red))
#		print(dim(tab2red))
#		print(counter)
#		print("Window")
#		print(i0)
#		print(i1)

		temp <- Checker(tab1red, tab2red, winvec, totalvec, counter)
		restab <- rbind(restab, temp$res)
		winvec <- temp$winvec
		totalvec <- temp$totalvec


###val1: stock on stock
###val2: stock on futures
###val3: futures on stock
###
		counter <- counter + 1
	}
list(res=restab, tab1=tab1red, tab2=tab2red, save1=save1, save2=save2, win=winvec, total=totalvec)
}
###���������, �������������� ������� ����������� ��������� DynamicTesting... $res
###����� ������ �� �����
Analysis <- function(tab)
{
	print("count")
	print(summary(tab[,2]))
	print("total")
	print(dim(tab)[1])
	print("stationarity")
	print(dim( tab[((tab[,3]<0.05) & (tab[,4]<0.05) & (tab[,5]<0.05) & (tab[,6]<0.05)
		& (tab[,7]>0.05) & (tab[,8]>0.05)),])[1])
	print(dim( tab[((tab[,5]<0.05) & (tab[,6]<0.05)
		& (tab[,7]>0.05) & (tab[,8]>0.05)),])[1])
	print("depends on futures")
	print(dim(tab[((tab[,9]<0.05) & (tab[,12]<0.05)),])[1])
	print("depends on stocks")
	print(dim(tab[((tab[,10]<0.05) & (tab[,11]<0.05)),])[1])
	print("both")
	print(dim(tab[((tab[,9]<0.05) & (tab[,10]<0.05)
			& (tab[,11]<0.05) & (tab[,12]<0.05)),])[1])
	print("information about lags")
	print("mean value of lags")
	print(mean(tab[,13]))
	print("median of lags")
	print(median(tab[,13]))
	print("deviation")
	print(sd(tab[,13]))
	print("information about kefs")
	print("stock to stock kef:")
	print(mean(tab[,14]))
	print(median(tab[,14]))
	print(sd(tab[,14]))
	print("stock to futures kef:")
	print(mean(tab[,15]))
	print(median(tab[,15]))
	print(sd(tab[,15]))
	print("futures to stock kef:")
	print(mean(tab[,16]))
	print(median(tab[,16]))
	print(sd(tab[,16]))
	print("futures to futures kef:")
	print(mean(tab[,17]))
	print(median(tab[,17]))
	print(sd(tab[,17]))
}
###������� ������ � �������� ���������
###����:
###str1 --- ����, �� �������� ������� ������� ������ ������
###str2 --- ����, �� �������� ������� ������� ������ ��������
###shift --- ������ ���� (1 --- ��� 1 ����, 5 --- ��� ���� ������ � �.�.)
###val --- ������ � ��������
###skip --- ���������� �������-�� ������
###breakpoint --- ���������������, ��������� ���� ������
###�����:
###������� ����������� ������������, �� ��� �������� Checker � ��������� ������
###����� ��� ������� ����� ������� � ������� Analysis
###�� ��������� �� ������� PREPAREDATA2 ��� �� ����� ���������
DynamicTesting <- function(str1="q1.txt", str2="q2.txt", shift=1, debug=0)
	{
	tab1 <- read.csv(str1,header=FALSE)
	tab2 <- read.csv(str2,header=FALSE)
	print("TOTAL DATA")
	print(dim(tab1))
	print(dim(tab2))
	tab1 <- PrepareData1(tab1)
	tab2 <- PrepareData1(tab2)
	restab <- data.frame(count=c(), dim=c(), bt1=c(), bt2=c(), 
			adf1=c(), adf2=c(), kpss1=c(), kpss2=c(), gt1=c(), gt2=c(), cas1=c(), cas2=c())
	shiftval <- 24*60*60
	i0 <- 0; i1 <- shiftval
	counter <- 1
		helper <- 5
		period <- 1
	while ((i0 <= max(tab1[,1])) & (i0 <= max(tab2[,1])))
	{
		tab1red <- data.frame(v1=c(), v2=c())
		tab2red <- tab1red
		j <- 1
		while (j<=shift)
		{
			print("index")
			print(j)
			print("helpdata")
			print(helper)
			print(period)
			helper <- helper + 1
			if (helper>7) {helper <- 1; period <- period + 1}
			
			tab1rednew <- tab1[(tab1[,1]>=i0) & (tab1[,1]<i1),]
			if (dim(tab1rednew)[1] == 0) 
			{
				i0 <- i1
				i1 <- i1 + shiftval
				if (i0 > max(tab1[,1])) {break}
				next
			}


			tab2rednew <- tab2[(tab2[,1]>=i0) & (tab2[,1]<i1),]

			tab1rednew <- PrepareData2(tab1rednew)
			tab2rednew <- PrepareData2(tab2rednew)

			print("current data")
			print(dim(tab1rednew))
			print(dim(tab2rednew))
			print(dim(tab1red))	
			hh <- FillData(tab1rednew, tab2rednew, group=0)

			print(dim(hh$tab1))
			print(dim(hh$tab2))

			names(hh$tab1) <- c("v1","v2")
			names(hh$tab2) <- c("v1","v2")

			if (debug==0)
			{

				datemin <- hh$tab1[2,1]
				datemax <- hh$tab1[dim(hh$tab1)[1],1]
				prev <- dim(hh$tab1)[1]
				datecut <- 30*60

				hh$tab1 <- hh$tab1[((hh$tab1[,1]>datemin+datecut)),]
				curr <- dim(hh$tab1)[1]
#			print("deleted")
#			print(curr-prev)
				prev <- curr
				hh$tab1 <- hh$tab1[((hh$tab1[,1]<datemax-datecut)),]
				hh$tab2 <- hh$tab2[((hh$tab2[,1]>datemin+datecut) & (hh$tab2[,1]<datemax-datecut)),]
				curr <- dim(hh$tab1)[1]
			}
#			print("deleted")
#			print(curr-prev)

#			print(names(hh$tab1))

#			print("here")
#			print(summary(tab1red))
#			print(summary(hh$tab1))

			tab1red <- rbind(tab1red, hh$tab1)
			tab2red <- rbind(tab2red, hh$tab2)

			i0 <- i1
			i1 <- i1 + shiftval

			j <- j + 1

		}

		if (dim(tab1red)[1] == 0) 
			{
#######				counter <- counter + 1
				i0 <- i1
				i1 <- i1 + shiftval
				next
			}

#		tab1red <- PrepareData2(tab1red)
#		tab2red <- PrepareData2(tab2red)
		tab1red[,2] <- log(tab1red[,2])
		tab2red[,2] <- log(tab2red[,2])
		print("CYCLE ENDED")
		print(dim(tab1red))
		print(dim(tab2red))
		print(counter)
		dimnum <- dim(tab1red)[1]
		temp <- Box.test(tab1red[,2],10,"Ljung-Box")
		bt1num <- temp$p.value
		temp <- Box.test(tab2red[,2],10,"Ljung-Box")
		bt2num <- temp$p.value
		temp <- adf.test(tab1red[,2], alternative="stationary")
		adf1num <- temp$p.value		
		temp <- adf.test(tab2red[,2], alternative="stationary")
		adf2num <- temp$p.value		
		temp <- kpss.test(tab1red[,2])
		kpss1num <- temp$p.value
		temp <- kpss.test(tab2red[,2])
		kpss2num <- temp$p.value
		hh <- cbind(tab1red[,2], tab2red[,2])
		temp <- granger.test(hh, p=10)
		gtnum1 <- temp[1,2]
		gtnum2 <- temp[2,2]
		temp <- causality(VAR(hh,10))
		cas1num <- temp$Granger$p.value
		hh <- cbind(tab2red[,2], tab1red[,2])
		temp <- causality(VAR(hh,10))
		cas2num <- temp$Granger$p.value
		
#		print(c(counter, dimnum, bt1num, bt2num, adf1num, adf2num, kpss1num,
#				kpss2num, gtnum, cas1num, cas2num))

###������� ������� ������ �����
###����� ����� ������ �������
###����� ����� ������ �������
###� ������� ������� ������ �����

		restab <- rbind(restab, c(counter, dimnum, bt1num, bt2num, adf1num, adf2num, kpss1num,
				kpss2num, gtnum1, gtnum2, cas1num, cas2num))

		counter <- counter + 1

######		if (counter>5) {break}

	}
restab
}
###not run
Initialize2 <- function()
{
	tab1 <- read.csv("q1.txt",header=FALSE)
	tab2 <- read.csv("q2.txt",header=FALSE)
	print(dim(tab1))
	tab1 <- tab1[tab1[,7]==0,]
	tab2 <- tab2[tab2[,7]==0,]
print(dim(tab1))
	tab1 <- PrepareData1(tab1)
	tab2 <- PrepareData1(tab2)
print(dim(tab1))
	tab1 <- cbind(tab1[,1], tab1[,2])
	tab2 <- cbind(tab2[,1], tab2[,2])
print(dim(tab1))
print(dim(tab2))
	tab1 <- PrepareData2(tab1)	
	tab2 <- PrepareData2(tab2)
	print("before")
	hh <- FillData(tab1, tab2)
	print("after")
	tab1 <- hh$tab1
	tab2 <- hh$tab2
	
	list(tab1=tab1, tab2=tab2)
}
###not run
Initialize <- function()
{
	tab1 <- read.csv("q1.txt",header=FALSE)
	tab2 <- read.csv("q2.txt",header=FALSE)
	tab1 <- tab1[tab1[,4]==0,]
	tab2 <- tab2[tab2[,4]==0,]
	tab1 <- PrepareData1(tab1)
	tab2 <- PrepareData1(tab2)
	print("before")
	hh <- FillData(tab1, tab2)
	print("after")
	tab1 <- hh$tab1
	tab2 <- hh$tab2
	tab1 <- PrepareData2(tab1)	
	tab2 <- PrepareData2(tab2)
	list(tab1=tab1, tab2=tab2)
}
###������� ��������� ������
###���������� ���� � ��������� ������� ������ ���
PrepareData1 <- function(tab)
{
	tab[,1] <- as.POSIXct(tab[,1])
####	tab[,1] <- as.numeric(tab[,1])
	tab[,1] <- as.numeric(tab[,1] - tab[1,1])
####	tab[,1] <- tab[,1] - tab[1,1]
#	vec <- c(tab[1,2],tab[1:(dim(tab)[1] - 1),2])
#	tvec <- tab[,2]/vec
#	tab[,2] <- tvec
	tab
}
PrepareData1mod <- function(tab)
{
	tab[,1] <- as.numeric(tab[,1])
	tab
}
###������� ��������� ������
###���������� ���������� ����������
###DynamicTestingSeconds � DynamicTesting
###�������, ��� ����������� ������������� ����������
PrepareData2 <- function(tab)
{
	vec <- c(tab[1,2],tab[1:(dim(tab)[1] - 1),2])
####	tvec <- tab[,2]/vec
	tvec <- tab[,2] - vec
	tab[,2] <- tvec
	tab
}
###������� ���������� ������������� �������
###tab1 --- ������, tab2 --- �������
###������ ���������� � ����� �����
###�� ������ ���� ����� �� ��������������� ����������
FillData <- function(tab1, tab2, group=0)
{
	tab1res <- data.frame(time=c(), val=c())
	tab2res <- tab1res
	i1 <- 1; i2 <- 1;
	tab1[i1,1] <- 0
	tab2[i2,1] <- 0
if (group==0)
{
	while ((i1<=dim(tab1)[1]) & (i2<=dim(tab2)[1]))
	{
		if (tab1[i1,1] == tab2[i2,1])
		{
			tab1res <- rbind(tab1res, c(tab1[i1,1], tab1[i1,2]))
			tab2res <- rbind(tab2res, c(tab2[i2,1], tab2[i2,2]))
			i1 <- i1 + 1
			i2 <- i2 + 1
		}			
		else if (tab1[i1,1]>tab2[i2,1])
		{
			tab1res <- rbind(tab1res, c(tab2[i2,1], tab1[i1-1,2]))
			tab2res <- rbind(tab2res, c(tab2[i2,1], tab2[i2,2]))
			i2 <- i2 + 1
		}
		else
		{
			tab1res <- rbind(tab1res, c(tab1[i1,1], tab1[i1,2]))
			tab2res <- rbind(tab2res, c(tab1[i1,1], tab2[i2-1,2]))
			i1 <- i1 + 1
		}
#####		if ((i1>100)) {break}
	}
}
else
{


}
	list(tab1 = tab1res, tab2 = tab2res)	
}

###pvalue --- ����������� ��������� H0 ��������,
###�.�. ����������� �������� ����� ���������� ��� H0
###�������� (F-Test)
###H0: ����� �� ������� ��� ���������
###p-value: 0.329
###Instant:
###H0: ��� ��������� �����
###p-value: 0.8913
###��������: 
###p-value 3.9*10^{-6}
###H0: �������� �� ������� ��� �����.
###� ��������� �������� � ������ �� ��������������

###�� �������� ��������
###> granger.test(hh,p=5)
###     F-statistic   p-value
### ->     1.211467 0.3007283
### ->    25.232264 0.0000000
###> granger.test(hh,p=10)
###     F-statistic   p-value
### ->    0.9364584 0.4979199
### ->   13.1790633 0.0000000
###> granger.test(hh,p=20)
###     F-statistic   p-value
### ->    0.8956809 0.5930959
### ->    7.0551838 0.0000000

###����� ��������� ������� �� �������� ��������
#> causality(h1)
#$Granger
#
#        Granger causality H0: y1 do not Granger-cause y2
#
#data:  VAR object h1
#F-Test = 25.2323, df1 = 5, df2 = 36796, p-value < 2.2e-16
#
#
#$Instant
#
#        H0: No instantaneous causality between: y1 and y2
#
#data:  VAR object h1
#Chi-squared = 6410.602, df = 1, p-value < 2.2e-16
#
#
#Warning message:
#In causality(h1) : 
#Argument 'cause' has not been specified;
#using first variable in 'x$y' (y1) as cause variable.
#
#> causality(h2)
#$Granger
#
#        Granger causality H0: y1 do not Granger-cause y2
#
#data:  VAR object h2
#F-Test = 1.2115, df1 = 5, df2 = 36796, p-value = 0.3007
#
#
#$Instant
#
#        H0: No instantaneous causality between: y1 and y2
#
#data:  VAR object h2
#Chi-squared = 6410.602, df = 1, p-value < 2.2e-16
#
#
#Warning message:
#In causality(h2) : 
#Argument 'cause' has not been specified;
#using first variable in 'x$y' (y1) as cause variable.
#




