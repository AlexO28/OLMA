Getsdinfo <- function(tab)
{
	restab <- rbind(c())
	days <- unique(tab$day)
	for (day in days)
	{
		tabred <- tab[tab$day==day,]
		vec <- (tabred$bid + tabred$ask)/2
		restab <- rbind(restab, rbind(c(day, sd(vec))))
	}
	restab
}
ModifyRColumn <- function(vec) {
  gsub(',', '.', substr(vec, 1, nchar(vec)-2))
}
GetRData <- function(storagepath, aname) {
  tab <- fread(paste(storagepath, paste0(aname, '.txt'), sep = "\\"), dec = ",", sep = "\t")
  print(head(tab))
  tab$V2 <- ModifyRColumn(tab$V2)
  tab$V1 <- sub('_.*', '', tab$V1)
  res <- data.table(cbind(tab$V1, tab$V2, tab$V6))
  names(res) <- c(paste(aname, "Ticker", sep = "."), paste(aname, "Prof", sep = "."), paste(aname, "NumDeals", sep = "."))
 res
}
GetFullData <- function(storagepath) {
cbind(GetRData(storagepath, '15'), GetRData(storagepath, '30'), GetRData(storagepath, '60'), GetRData(storagepath, '90'),
GetRData(storagepath, '120'), GetRData(storagepath, '180'), GetRData(storagepath, '240'), GetRData(storagepath, '300'), GetRData(storagepath, '480'))

}
ModifyStrToNiceFormat <- function(vec) {
  vec <- substr(vec, 3, nchar(vec)-2)
  vec <- gsub("'", '', vec)
  vec <- gsub(' ', '', vec)
  vec <- gsub(',', '-', vec)
vec  
}
GetRandomSubSample <- function(tab) {
  inds1 <- which(tab$cluster == 1)
  inds4 <- which(tab$cluster == 4)
  inds5 <- which(tab$cluster == 5)
  inds6 <- which(tab$cluster == 6)
  print(length(inds1))
  print(length(inds4))
  print(length(inds5))
  print(length(inds6))
  rand1 <- sample(inds1, 88)
  rand4 <- sample(inds4, 4)
  rand5 <- sample(inds5, 9)
  rand6 <- sample(inds6, 99)
list(rand1, rand4, rand5, rand6)  
}