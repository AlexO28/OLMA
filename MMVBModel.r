Model <- function(Date = Sys.Date(), Type = "M5", num = 5, K = 0.00002, N = 7, src = "Finam", dir = NA) {
  for (j in N:1) {
    print(paste("Information for ", Date - j + 1))
    res <- MModel(Date = Date - j, Type = Type, num = num, K = K, src = src, dir = dir)
    print(res)
  }
}

MModel <- function(Date = Sys.Date()-1, Type = "M5", num=5, K = 0.00002, printmode=TRUE, src = "Finam", dir = NA) {
#получение данных ММВБ
  ###dirstr <- "\\\\192.168.1.12\\historical_data\\PLAZA\\best price 1m\\"
  if (is.na(dir)) {
    if (src == "Plaza") {
      dirstr <- "\\\\192.168.1.12\\historical_data\\PLAZA\\bp\\1m\\"
    } else {
      dirstr <- "\\\\192.168.1.12\\historical_data\\FINAM\\1m\\"
    }
  } else {dirstr <- dir}
  dirstrMMVB <- paste0(dirstr, "MX", Type, "@FORTS\\")

  dates <- c()
  totalcount <- 0
  tabMMVB <- rbind(c(), c())
  while (totalcount < num)
  {
    strdate <- DateToStr(Date)
    if (src == "Plaza") {
      fileMMVB <- paste0(dirstrMMVB, "depths_MX", Type, "@FORTS_", strdate, ".csv")
    } else {
      fileMMVB <- paste0(dirstrMMVB, "candles_TimeFrameCandle_00_01_00_MX", Type, "@FORTS_", strdate, ".csv")
    }

    if (file.exists(fileMMVB)) {
      ####print(fileMMVB)
      tab <- read.table(fileMMVB, header = TRUE, sep = ";")
      tab[, 1] <- as.POSIXct(tab[, 1])
      if (src == "Plaza") {
        tab <- tab[((tab$Bid>0) & (tab$Ask>0)), ]
        tab2 <- data.frame(Time = tab$Time, Close = (tab$Bid + tab$Ask)/2)
      } else {
        tab2 <- data.frame(Time = tab$Time, Close = tab$Close)
      }
      
      datestart <- paste(strftime(tab2$Time[1], format = "%Y-%m-%d"), "10:30:00")
      dateend <- paste(strftime(tab2$Time[1], format = "%Y-%m-%d"), "18:30:00")      
      tab2 <- tab2[((tab2$Time >= as.POSIXct(datestart)) & (tab2$Time <= as.POSIXct(dateend))), ]

      tabMMVB <- rbind(tabMMVB, tab2)
      totalcount <- totalcount + 1
      dates <- c(dates, strdate)
    }
    else {
     #print("NO")
    }
    Date <- Date - 1;
  }  
  if (printmode) {
    print("Used data from dates:")
    print(dates)
  }
  tabMMVB <- tabMMVB[order(tabMMVB$Time), ]
#получение данных по РТС и доллару
  tabRTS <- rbind(c(), c())
  tabSI <- rbind(c(), c())
  dirstrSI <- paste0(dirstr, "SI", Type, "@FORTS\\")
  dirstrRTS <- paste0(dirstr, "RI", Type, "@FORTS\\")
  for (j in 1:num) {
     strdate <- dates[j]
     if (src == "Plaza") {
       fileSI <- paste0(dirstrSI, "depths_SI", Type, "@FORTS_", strdate, ".csv") 
     } else {
       fileSI <- paste0(dirstrSI, "candles_TimeFrameCandle_00_01_00_SI", Type, "@FORTS_", strdate, ".csv") 
     }
     if (file.exists(fileSI))  {
       tab <- read.table(fileSI, header = TRUE, sep = ";")
       tab[, 1] <- as.POSIXct(tab[, 1])
       if (src == "Plaza") {
         tab <- tab[((tab$Bid>0) & (tab$Ask>0)), ]
         tab2 <- data.frame(Time = tab$Time, Close = (tab$Bid + tab$Ask)/2)
       } else {
         tab2 <- data.frame(Time = tab$Time, Close = tab$Close)
       }
       datestart <- paste(strftime(tab2$Time[1], format = "%Y-%m-%d"), "10:30:00")
       dateend <- paste(strftime(tab2$Time[1], format = "%Y-%m-%d"), "18:30:00")      
       tab2 <- tab2[((tab2$Time >= as.POSIXct(datestart)) & (tab2$Time <= as.POSIXct(dateend))), ]

       tabSI <- rbind(tabSI, tab2)   
     }
     else {
       print(strdate)
       print(paste("Error! File not found for SI! The date is "), as.character(strdate))
     }
     if (src == "Plaza") {
       fileRTS <- paste0(dirstrRTS, "depths_RI", Type, "@FORTS_", strdate, ".csv") 
     } else {
       fileRTS <- paste0(dirstrRTS, "candles_TimeFrameCandle_00_01_00_RI", Type, "@FORTS_", strdate, ".csv")
     }
     if (file.exists(fileRTS))  {
       tab <- read.table(fileRTS, header = TRUE, sep = ";")
       tab[, 1] <- as.POSIXct(tab[, 1])
       if (src == "Plaza") {
         tab <- tab[((tab$Bid>0) & (tab$Ask>0)), ]
         tab2 <- data.frame(Time = tab$Time, Close = (tab$Bid + tab$Ask)/2)
       } else {
         tab2 <- data.frame(Time = tab$Time, Close = tab$Close)
       }
       datestart <- paste(strftime(tab2$Time[1], format = "%Y-%m-%d"), "10:30:00")
       dateend <- paste(strftime(tab2$Time[1], format = "%Y-%m-%d"), "18:30:00")      
       tab2 <- tab2[((tab2$Time >= as.POSIXct(datestart)) & (tab2$Time <= as.POSIXct(dateend))), ]

       tabRTS <- rbind(tabRTS, tab2)   
     }
     else {
       print(strdate)
       print(paste("Error! File not found for SI! The date is "), strdate)
     }
  }
  tabSI <- tabSI[order(tabSI$Time), ]
  tabRTS <- tabRTS[order(tabRTS$Time), ]

#фильтрация
  tab <- tabMMVB[((tabMMVB$Time %in% tabSI$Time) & (tabMMVB$Time %in% tabRTS$Time)), ]
  tab1 <- tabRTS[((tabRTS$Time %in% tabSI$Time) & (tabRTS$Time %in% tabMMVB$Time)), ]
  tab2 <- tabSI[((tabSI$Time %in% tabRTS$Time) & (tabSI$Time %in% tabMMVB$Time)), ]
  tabMMVB <- tab
  tabRTS <- tab1
  tabSI <- tab2

  ###print(c(dim(tabMMVB)[1], dim(tabRTS)[1], dim(tabSI)[1]))
#регрессия
  vec1 <- tabMMVB$Close
  vec2 <- tabRTS$Close * (tabSI$Close * K)

lm(vec1 ~ vec2)$coefficients
}

DateToStr <- function(Date) {
  day <- as.numeric(strftime(Date, format = "%d"))
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y"))
####c(year, NumToChar(month), NumToChar(day))
  paste0(NumToChar(year), "_", NumToChar(month), "_", NumToChar(day))
}

NumToChar <- function(num) {
  if (num < 10) {
     res <- paste0("0", as.character(num))
  } else {
     res <- as.character(num)
  }
res
}

ChangeSeparator <- function(file, sepprev, sepcur) {
  tab <- read.table(file, header = TRUE, sep = sepprev)
  tab$Vol <- NULL 
  write.table(tab, file, append = FALSE, row.names = FALSE, sep = sepcur)
}
