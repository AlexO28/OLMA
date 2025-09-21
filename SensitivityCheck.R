###requires MysteryOf4Numbers.R
###quotes have capital letters in names, orders have small letters in names
GetSensitiveChanges <- function(tab, timestart, timeend, sensitivity, name) {
#find initial price
  tabred <- tab[(tab$Time >= timestart - 0.01) & (tab$Time <= timestart), ]
  startprice <- tab[nrow(tab), name]
  starttime <- timestart
  tab <- tab[(tab$Time >= timestart) & (tab$Time <= timeend), ]
  tab$id <- 1:nrow(tab)
#main loop
  restab <- data.frame(time = numeric(), price = numeric())
  for (j in 1:nrow(tab)) {
    if (abs(tab[j, name] - startprice) >= sensitivity) {
      startprice <- tab[j, name]
      restab <- rbind(restab, data.frame(Time = tab$Time[j], price = startprice))
    }
  }
restab
}
JoinAndGetTimeDiff <- function(tab, orders, dir0) {
  names(orders) <- tolower(names(orders))
  if (!is.na(dir0)) {
    orders <- orders[orders$direction == dir0, ]
  }
  tab$id <- 1:nrow(tab)
  indices <- unlist(GetInfoByTimes(orders, tab, securname = "Time", simplemode = 0))
  print(length(indices))
  joinedtab <- cbind(orders[!is.na(indices), ], tab[indices[!is.na(indices)], ])
  joinedtab$diff <-as.numeric(joinedtab$time - joinedtab$Time)
joinedtab
}
AddMSToTime <- function(tab) {
  tab$MS <- tab$MS %/% 1000
  abnormal <- which(tab$MS < 10)
  tab$MS[-abnormal] <- ifelse(tab$MS[-abnormal] >= 100, as.character(tab$MS[-abnormal]), paste0("0", as.character(tab$MS[-abnormal])))
  tab$MS[abnormal] <- paste0("00", as.character(tab$MS[abnormal]))
  date <- Sys.Date()
  print(date)
  tab$Date <- paste0(date, " ", tab$Time, ".", tab$MS)
data.frame(Time = as.POSIXct(tab$Date), Direction = tab$Direction)
}

