library(data.table)
library(fasttime)

GetDroninData <- function(tab) {
  tab <- as.data.table(tab)
  names(tab) <- c("Volume", "Date1", "Price", "Time1", "Date2")
  tab[, Date1 :=  substr(Date1, 1, 10)]
  tab[, Time1 := substr(Time1, 12, 19)]
  tab[, mydt := fastPOSIXct(paste(Date1, Time1))]
  setkey(tab, "mydt")
tab
}

CompareTwoVecs <- function(tab) {
  vec1 <- as.numeric(tab$Price[2:nrow(tab)])-as.numeric(tab$Price[1:(nrow(tab)-1)])
  vec2 <- as.numeric(tab$mydt[2:nrow(tab)])-as.numeric(tab$mydt[1:(nrow(tab)-1)])
data.table(mydt = tab$mydt[1:(nrow(tab)-1)], pricediff = vec1, timediff = vec2)
}

CalculateReturnability <- function(vectab, tab) {
  vectab$deltatime <- 0
  tab$Price <- as.numeric(tab$Price)
  vectab$price0 <- 0 
  vectab$price1 <- 0 
  vectab$price3 <- 0
  for (j in 1:nrow(vectab)) {
    mydt0 <- vectab$mydt[j]
	mypricechange <- vectab$pricediff[j]
	#print(mydt0)
	tabred <- tab[mydt>=mydt0, ]
	price0 <- tabred$Price[1]
	#print(price0*101)
	time0 <- as.numeric(tabred$mydt[2])
	if (mypricechange>0) {
	  tabred2 <- tab[(mydt>mydt0) & (Price<=price0+0.00001)]
	  deltatime <- as.numeric(tabred2$mydt[1]) - time0
	} else {
	  tabred2 <- tab[(mydt>mydt0) & (Price>=price0-0.00001)]
	  deltatime <- as.numeric(tabred2$mydt[1]) - time0	
	}
	vectab$deltatime[j] <- deltatime
	vectab$price0[j] <- price0
	vectab$price1[j] <- tabred$Price[2]
#if (nrow(tabred2)>0) {
	vectab$price3[j] <- ifelse(nrow(tabred2)>0, tabred2$Price[1], NA)
	#}
  }
 vectab
}