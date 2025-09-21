library(MASS)

GetGroupTab <- function(vec, num) {
  setwd("C:\\OLMA\\Data")
  tabres <- rbind(c(), c())
  for (day in vec) {
    str <- paste0("mylog", num, "_", day, ".txt")
    tab <- read.table(str, header = FALSE, sep = ",")
    tab <- as.data.frame(tab)
    names(tab) <- c("day", "v0", "v1", "v2", "v3", "fmin", "fmax")
    tab <- tab[tab$v1 != 0, ]
    tabres <- rbind(tabres, tab)
  }
  setwd("C:\\OLMA\\")
tabres
}

GroupCheck <- function(vec, num, minval1, minval2, maxval1, maxval2, crvals) {
  setwd("C:\\OLMA\\Data")
  tabres <- rbind(c(), c())
  for (crval in crvals) {
    num1 <- 0; num2 <- 0; num3 <- 0; num4 <- 0; num5 <- 0; num6 <- 0
    for (day in vec) {
      str <- paste0("mylog", num, "_", day, ".txt")
      tab <- read.table(str, header = FALSE, sep = ",")
      tab <- as.data.frame(tab)
      names(tab) <- c("day", "v0", "v1", "v2", "v3", "fmin", "fmax")
      tab <- tab[tab$v1 != 0, ]
      tabred <- tab[((tab$v1 <= minval2) & (tab$v1 >= minval1)), ]
      num1 <- num1 + dim(tabred)[1]
      num2 <- num2 + dim(tabred[tabred$fmin >= crval, ])[1]
      num3 <- num3 + dim(tabred[tabred$fmax >= crval, ])[1]        
      tabred <- tab[((tab$v1 >= maxval1) & (tab$v1 <= maxval2)), ]
      num4 <- num4 + dim(tabred)[1]
      num5 <- num5 + dim(tabred[tabred$fmin >= crval, ])[1]
      num6 <- num6 + dim(tabred[tabred$fmax >= crval, ])[1]        
    }
    tabres <- rbind(tabres, rbind(c(num1, num2/num1, num3/num1, num2/num1 - num3/num1, num4, num5/num4, num6/num4, num6/num4 - num5/num4)))
  }
  setwd("C:\\OLMA\\")
tabres
}

DrawPart <- function(tab, i, flag)
{
if (flag==0)
{
	tabred <- tab[tab[,5]==i,] 
	print(dim(tabred)[1])
	print(summary(tabred[,4]-tabred[,3]))
	truehist(tabred[,4]-tabred[,3])
}
else if (flag==1)
{
	tabred <- tab[tab[,6]==i,] 
	print(dim(tabred)[1])
	print(summary(tabred[,4]-tabred[,3]))
	truehist(tabred[,4]-tabred[,3])
}
else if (flag==2)
{
	tabred <- tab[tab[,5]==i,] 
	print(dim(tabred)[1])
	print(summary(tabred[,4]+tabred[,3]))
	truehist(tabred[,4]+tabred[,3])
}
else if (flag==3)
{
	tabred <- tab[tab[,6]==i,] 
	print(dim(tabred)[1])
	print(summary(tabred[,4]+tabred[,3]))
	truehist(tabred[,4]+tabred[,3])
}
}

ClusterAnalysis <- function(flag, vec, size)
{
  wss <- c()
  for (j in 1:size) {
    wss <- c(wss, sum(kmeans(vec, centers = j, algorithm="Lloyd", iter.max=100)$withinss))
  }
  if (flag==1) {
    plot(1:size, wss, type = "b")
  } else if (flag == 2) {
    plot(2:size, wss[2:size], type = "b")
  }
}

GetClusteredInfo <- function(tab)
{
  totalnum <- dim(tab)[1]
  centers <- unique(tab$cluster)
  centers <- centers[order(centers)]
  restab <- rbind(c(), c())
  for (j in 1:length(centers)) {
    tabred <- tab[tab$cluster == j, ]
    print(c(j, dim(tabred)[1]))
    print(summary(tabred$val))
    restab <- rbind(restab, rbind(c(), c(j, dim(tabred)[1]/totalnum, min(tabred$val), median(tabred$val), mean(tabred$val), max(tabred$val)))) 
  }
  restab <- as.data.frame(restab)
  names(restab) <- c("cluster", "prob", "min", "median", "mean", "max")
  restab <- restab[order(restab$prob, decreasing = TRUE), ]
restab
}

GetInfoAlt <- function(tab, vec, vec1, vec2)
{
    tab <- cbind(tab, vec)
	tabred <- tab[((tab[,6]>vec1[1]) & (tab[,6]<vec1[2])),]
	print(dim(tabred[1]))
    print(summary(tabred[, 5]))
    print(summary(tabred[, 4]))
    print(c(mean(tabred[,5])-mean(tab[,5]), median(tabred[,5] - median(tab[,5]))))
    print(c(mean(tabred[,4])-mean(tab[,4]), median(tabred[,4] - median(tab[,4]))))
    print(dim(tabred[tabred[,5]>=5,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=10,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=15,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=20,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=30,])[1]/dim(tabred)[1])
print("***")
 print(dim(tabred[tabred[,4]>=5,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=10,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=15,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=20,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=30,])[1]/dim(tabred)[1])
	tabred <- tab[((tab[,6]>vec2[1]) & (tab[,6]<vec2[2])),]
	print(dim(tabred[1]))
    print(summary(tabred[, 5]))
    print(summary(tabred[, 4]))
    print(c(mean(tabred[,5])-mean(tab[,5]), median(tabred[,5] - median(tab[,5]))))
    print(c(mean(tabred[,4])-mean(tab[,4]), median(tabred[,4] - median(tab[,4]))))
    print(dim(tabred[tabred[,4]>=5,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=10,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=15,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=20,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,4]>=30,])[1]/dim(tabred)[1])
print("***")
    print(dim(tabred[tabred[,5]>=5,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=10,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=15,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=20,])[1]/dim(tabred)[1])
    print(dim(tabred[tabred[,5]>=30,])[1]/dim(tabred)[1])

}

GetInfoByCluster <- function(tab, clust, crval) {
  tab <- tab[tab$cluster == clust, ]
  num1 <- dim(tab)[1]
  num2 <- dim(tab[tab$fmin >= crval, ])[1]
  num3 <- dim(tab[tab$fmax >= crval, ])[1]
  print(num1)
  print(num2/num1)
  print(num3/num1)
}