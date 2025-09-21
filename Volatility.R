MyGetVolatilityServe <- function(adate, tab, len) {
  tabred  <- tab[Date == adate, ]
sd(tabred[seq(1, nrow(tabred), len), stavka])
}
MyGetVolatility <- function(tab, len) {
data.frame(Date = unique(tab$Date), Val = sapply(unique(tab$Date), "MyGetVolatilityServe", tab = tab, len = len))
}
#TimeNum --- numeric version of Time
EstimateVolatilityForTimeMoment1 <- function(tab, atime, loclen = 300, globlen = 600, aname) {
  atimenum <- as.numeric(as.POSIXct(atime))
  tabred <- tab[TimeNum <= atimenum & TimeNum >= atimenum - globlen, ]
  inds <- which(atimenum - tabred$TimeNum >= loclen)
  if (length(inds) == 0) {return(0)}
 mean(tabred[inds, aname, with = FALSE] - tabred[inds + loclen, aname, with = FALSE])^2
}
GlobalEstimateVolatilityForTimeMoment1 <- function(tab, aname, loclen = 300) {
  vecend <- tab[(loclen + 1):nrow(tab), aname, with = FALSE]
  vecstart <- tab[1:(nrow(tab) - loclen), aname, with = FALSE]
(vecend - vecstart)^2
}