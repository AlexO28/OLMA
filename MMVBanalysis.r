library(MASS)

ParseLevels <- function(tab1, tab2) {
  tab3 <- read.table("level1H.csv", header = TRUE, sep = ";")
  tab3[, 1] <- as.POSIXct(tab3[, 1])
  temptab <- read.table("level1M.csv", header = TRUE, sep = ";")
  temptab[, 1] <- as.POSIXct(temptab[, 1])
  temptab <- temptab[as.numeric(temptab[, 1]) > max(as.numeric(tab3[, 1])), ]
  tab3 <- rbind(tab3, temptab)
  names(tab3) <- c("Time", "Close")
  tab3 <- tab3[order(tab3$Time), ]
  
  tab1 <- tab1[as.numeric(tab1$Time) >= min(as.numeric(tab3$Time)), ]
  tab2 <- tab2[as.numeric(tab2$Time) >= min(as.numeric(tab3$Time)), ]
  levtab <- rbind(c(), c())

  j0 <- 1
  for (j in 1:dim(tab2)[1]) {
     if (j0 == dim(tab3)[1]) {
        levtab <- rbind(levtab, c(as.POSIXct(tab2$Time[j]), tab3$Close[j0], tab2$Yday[j]))
     } else {
        while (1==1) {
          if (as.numeric(tab3$Time[j0+1]) > as.numeric(tab2$Time[j])) {
            break
          } else {
            j0 <- j0 + 1
            if (j0 == dim(tab3)[1]) {break}
          }
        }
        levtab <- rbind(levtab, c(as.POSIXct(tab2$Time[j]), tab3$Close[j0], tab2$Yday[j]))
     }
  }
  levtab <- as.data.frame(levtab)
  names(levtab) <- c("Time", "Close", "Yday")
list(tab1 = tab1, tab2 = tab2, tab3 = levtab)
}

GetDataFull <- function() {
  tab2 <- read.table("spreadsMXH5.csv", header=TRUE, sep=";")
  tab2[, 1] <- as.POSIXct(tab2[, 1])
  tab <- read.table("spreadsMXM5.csv", header=TRUE, sep=";")
  tab[, 1] <- as.POSIXct(tab[, 1])
  tab <- tab[as.numeric(tab[,1])> max(as.numeric(tab2[,1])),]
  restab1 <- rbind(tab2[,1:3], tab[,1:3])
  restab <- restab1[order(restab1$Time), ]
  tab2 <- read.table("spreadsRIH5.csv", header=TRUE, sep=";")
  tab2[, 1] <- as.POSIXct(tab2[, 1])
  tab <- read.table("spreadsRIM5.csv", header=TRUE, sep=";")
  tab[, 1] <- as.POSIXct(tab[, 1])
  tab <- tab[as.numeric(tab[,1])> max(as.numeric(tab2[,1])),]
  restab2 <- rbind(tab2[,1:3], tab[,1:3])
  restab2 <- restab2[order(restab2$Time), ]
  tab2 <- read.table("spreadsSIH5.csv", header=TRUE, sep=";")
  tab2[, 1] <- as.POSIXct(tab2[, 1])
  tab <- read.table("spreadsSIM5.csv", header=TRUE, sep=";")
  tab[, 1] <- as.POSIXct(tab[, 1])
  tab <- tab[as.numeric(tab[,1])> max(as.numeric(tab2[,1])),]
  restab3 <- rbind(tab2[,1:3], tab[,1:3])
  restab3 <- restab3[order(restab3$Time), ]
  tab <- read.table("spreadsUSD.csv", header=TRUE, sep=";")
  tab[, 1] <- as.POSIXct(tab[, 1])
  restab4 <- tab[, 1:3]
  list(tab1=restab1, tab2=restab2, tab3=restab3, tab4=restab4)
}

PrepareData <- function(tab1, tab2, tab3) {
  tab1 <- tab1[((tab1$Bid>0) & (tab1$Ask>0)), ]
  tab1 <- tab1[abs(tab1$Bid - tab1$Ask)<100, ]
  tab1temp <- tab1[((tab1$Time %in% tab2$Time) & (tab1$Time %in% tab3$Time)), ]
  tab2temp <- tab2[((tab2$Time %in% tab1$Time) & (tab2$Time %in% tab3$Time)), ]
  tab3temp <- tab3[((tab3$Time %in% tab2$Time) & (tab3$Time %in% tab1$Time)), ]  

  tab1 <- tab1temp; tab2 <- tab2temp; tab3 <- tab3temp

print(c(dim(tab1)[1], dim(tab2)[1], dim(tab3)[1]))

  tab1 <- data.frame(Time = tab1$Time, Close = (tab1$Bid + tab1$Ask)/2, Yday =  as.POSIXlt(tab1$Time)$yday)
  tab2 <- data.frame(Time = tab2$Time, Close = (tab2$Bid + tab2$Ask)/2, Yday =  as.POSIXlt(tab2$Time)$yday)
  tab3 <- data.frame(Time = tab3$Time, Close = (tab3$Bid + tab3$Ask)/2, Yday =  as.POSIXlt(tab3$Time)$yday)
list(tab1 = tab1, tab2 = tab2, tab3 = tab3)
}

GetData <- function(str, flag)  {
  setwd("C:\\OLMA\\Data")
  tab <- read.table(str, header=TRUE, sep = ";")
  ###tab <- tab[2:dim(tab)[1],]
  tab[, 1] <- as.POSIXct(tab[,1])
  for (j in 2:6) {
     tab[, j] <- as.numeric(tab[, j])
  }
print(summary(tab))
if (flag==1) {
  tab <- tab[tab[, 1] >= as.POSIXct("2015-01-01 00:00:00"), ]
}
else {
  tab <- tab[tab[, 1] <= as.POSIXct("2015-01-01 00:00:00"), ]
}
  ###tab <- tab[tab[, 1] <= as.POSIXct("2015-01-01 00:00:00"), ]
  vec <- as.POSIXlt(tab[, 1])$yday
  tab <- cbind(tab, vec)
  tab <- as.data.frame(tab)
  names(tab) <- c("Time", "Open", "High", "Low", "Close", "Volume", "Yday")
tab  
}

SeparateTabs <- function(tab1, tab2, tab3, tab4, vec, indik)  {
print(summary(tab1))
print(vec[indik == 1])
  tab1test <- tab1[tab1$Yday %in% vec[indik == 1],]  
print(summary(tab1test))
  tab2test <- tab2[tab2$Yday %in% vec[indik == 1],]  
  tab3test <- tab3[tab3$Yday %in% vec[indik == 1],]
  tab4test <- tab4[tab4$Yday %in% vec[indik == 1],]  
  tab1check <- tab1[tab1$Yday %in% vec[indik == 0],]
  tab2check <- tab2[tab2$Yday %in% vec[indik == 0],]
  tab3check <- tab3[tab3$Yday %in% vec[indik == 0],]
  tab4check <- tab4[tab4$Yday %in% vec[indik == 0],]  
list (test1 = tab1test, test2 = tab2test, test3 = tab3test, test4 = tab4test, check1 = tab1check, check2 = tab2check, check3 = tab3check, check4 = tab4check)
}

SetOpenDayPos <- function(tab) {
  tab$pos <- 0
  print(is.data.frame(tab))
  print(summary(tab))
  vec <- unique(tab$Yday)
  for (v in vec) {
     tabred <- tab[tab$Yday == v, ]
     num <- tabred$Open[1]
     tab$pos[tab$Yday == v] <- num
  }
tab
}

ModelDraw <- function(vec1, vec2, str, kefs, flag, day) {
  png(paste0(str, "plot", day, ".png"))
  plot(vec2, vec1)
  if (flag == 1) {
    abline(kefs[1], kefs[2], col="red")
abline(kefs[1]+100, kefs[2], col="green")
abline(kefs[1]-100, kefs[2], col="green")
abline(kefs[1]+200, kefs[2])
abline(kefs[1]-200, kefs[2])
abline(kefs[1]+400, kefs[2], col="blue")
abline(kefs[1]-400, kefs[2], col="blue")

  }
  else  {
    abline(0, kefs)
  }
  dev.off()
}

GetModelByDay <- function(tab1, tab2, tab3, day, flag) {
  tab1red <- tab1[tab1$Yday == day, ]
  tab2red <- tab2[tab2$Yday == day, ]
  tab3red <- tab3[tab3$Yday == day, ]
  vec1 <- tab1red$Close
  vec2 <- tab2red$Close * tab3red$Close
  if (flag==1)
  {
    haha <- lm(vec1 ~ vec2)
  ###  ModelDraw(vec1, vec2, "lm", haha$coefficients, flag, day)
  }
  else if (flag==0)
  {
    haha <- lm(vec1 ~ vec2 + 0)
   # ModelDraw(vec1, vec2, "lm", haha$coefficients, flag, day)
  }
####print(haha)
haha
}

GetModelByDayDollar <- function(tabdol, tabbrent, flag, num, drawmode) {
  vec1 <- tabbrent$Close
  vec2 <- 1/tabdol$Close
  if (flag == 1) {
     haha <- lm(vec1 ~ vec2)
  } 
  else if (flag == 0) {
    haha <- lm(vec1 ~ vec2 + 0)
  }
  kefs <- haha$coefficients
  if (drawmode) {
    png(paste0("br", num, ".png"))
    plot(vec2, vec1)
    if (flag == 1) {
      abline(kefs[1], kefs[2], col="red")
    } else if (flag == 0) {
      abline(0, kefs, col="red")
    }
    dev.off()
  }
  if (flag == 1) {
    error <- vec1 - vec2*kefs[2] - kefs[1]
  }
list(kefs = kefs, error = error)
}

CheckModelDollar <- function(tabdol, tabbrent) {
  setwd("C:\\OLMA\\FunnyPictures\\")
  days <- unique(tabdol$Yday)
  res <- rbind(c(), c())
  kres <- rbind(c(), c())
  for (day in days) {
    temp <- GetModelByDayDollar(tabdol[tabdol$Yday == day, ], tabbrent[tabbrent$Yday == day, ], 1, day, TRUE)
    res <- rbind(res, rbind(c(), c(min(abs(temp$error)), median(abs(temp$error)), mean(abs(temp$error)), max(abs(temp$error)))))
    kres <- rbind(kres, rbind(c(), temp$kefs))
  }
  setwd("C:\\OLMA\\")
kres
}

MyDraw <- function(str, vec1, vec2, kefs) {
  png(str)
  plot(vec2, vec1, ylim = c(mean(vec1)-2.5, mean(vec1)+2.5))
  abline(kefs[1], kefs[2], col="red")
  dev.off()
}

CheckModelDollarWeek <- function(tabdol, tabbrent, skip) {
  setwd("C:\\OLMA\\FunnyPictures\\")
  days <- unique(tabdol$Yday)
  res <- rbind(c(), c())
  kres <- rbind(c(), c())
  for (j in (skip+1):length(days)) {
    day <- days[j]
    daysprev <- days[((j-skip):(j-1))]
    temp <- GetModelByDayDollar(tabdol[tabdol$Yday %in% daysprev, ], tabbrent[tabbrent$Yday %in% daysprev, ], 1, day, FALSE)
    ###res <- rbind(res, rbind(c(), c(min(abs(temp$error)), median(abs(temp$error)), mean(abs(temp$error)), max(abs(temp$error)))))
    vec1 <- tabbrent$Close[tabbrent$Yday == day]
    vec2 <- 1/(tabdol$Close[tabdol$Yday == day])
    errors <- vec1 - temp$kefs[1] - temp$kefs[2] * vec2
    res <- rbind(res, rbind(c(), c(min(abs(errors)), median(abs(errors)), mean(abs(errors)), max(abs(errors)))))
    kres <- rbind(kres, rbind(c(), temp$kefs))   
    MyDraw(paste0("br", skip,"_", day, ".png"), vec1, vec2, temp$kefs) 
  }
  setwd("C:\\OLMA\\")
list(kres=kres, res=res)
}

GetModelData <- function(tab1, tab2, tab3, days, flag) {
  res <- rbind(c(), c())
  for (day in days) {
    temp <- GetModelByDay(tab1, tab2, tab3, day, flag)
    res <- rbind(res, c(day, temp$coefficients))
  }
res
}

CheckModel <- function(tab1, tab2, tab3, kefs, flag) {
res <- rbind(c(), c())
  days <- unique(tab1$Yday)
  days <- days[order(days)]
  for (j in 1:(length(days)-1)) {
     temp <- GetModelByDay(tab1, tab2, tab3, days[j], flag)
     kefsprev <- temp$coefficients
###     kefscur <- kefs$coefficients[1] + kefs$coefficients[2] * kefsprev
     kefscur <- kefsprev
     kefscur <- c(1.663752e+03, 3.149544e-05)
####print(c(kefscur-kefsprev, kefscur))
     tab1red <- tab1[tab1$Yday == days[j+1], ]
     tab2red <- tab2[tab2$Yday == days[j+1], ]
     tab3red <- tab3[tab3$Yday == days[j+1], ]
     vec1 <- tab1red$Close
     vec2 <- tab2red$Close * tab3red$Close
     print(c(days[j+1], max(abs(vec1 - kefscur[2]*vec2 - kefscur[1]))))
res <- rbind(res, max(abs(vec1 - kefscur[2]*vec2 - kefscur[1])))
     ModelDraw(vec1, vec2, "lm", kefscur, flag, days[j+1])
  }
res
}

CheckModelWeek <- function(tab1, tab2, tab3, skip) {
  days <- unique(tab1$Yday)
  days <- days[order(days)]
  for (j in (skip+1):(length(days))) {
tab1red <- 0
     daysred <- days[(j-skip):(j-1)]
     tab1red <- tab1[tab1$Yday %in% days[daysred], ]
     tab2red <- tab2[tab2$Yday %in% days[daysred], ]
     tab3red <- tab3[tab3$Yday %in% days[daysred], ]
print("***")
print(daysred)
print(summary(tab1red))
print(summary(tab1[tab1$Yday %in% daysred,]))
print(summary(tab2red))
print(summary(tab3red))
if (dim(tab1red)[1]>0)
{
     tab1red$Yday <- 0
     tab2red$Yday <- 0
     tab3red$Yday <- 0
     kefs <- GetModelByDay(tab1red, tab2red, tab3red, 0, 1)$coefficients  
#####kefs[2] <- kefs[2]/0.00002
print(kefs)   
     tab1red <- tab1[tab1$Yday == days[j], ]
     tab2red <- tab2[tab2$Yday == days[j], ]
     tab3red <- tab3[tab3$Yday == days[j], ]
     vec1 <- tab1red$Close
     vec2 <- tab2red$Close * tab3red$Close
     print(days[j])
  #   print(summary(vec1))
  #   print(summary(vec2))
     ModelDraw(vec1, vec2, "lmweek", kefs, 1, days[j])
}
  }
}

GetErrors <- function(tab1, tab2, tab3, flag) {
  days <- unique(tab1$Yday)
  days <- days[order(days)]
  xres <- c()
  yres <- c()
  zres <- c()
  zzres <- c()  
  for (j in (2:length(days)))
    {
       vec1prev  <- tab1$Close[tab1$Yday == days[j-1]]
       vec2prev  <- tab2$Close[tab2$Yday == days[j-1]] * tab3$Close[tab3$Yday == days[j-1]]
       if (flag == 1)  {
         kefs <- GetModelByDay(tab1, tab2, tab3, days[j-1], 1)$coefficients
       }
       else {
         ##### vec1prev <- vec1prev - vec1prev[1] + vec2prev[1]
          kefs <- lm(vec1prev ~ vec2prev + 0)$coefficients
       }
       vec1 <- tab1$Close[tab1$Yday == days[j]]
       vec2 <- tab2$Close[tab2$Yday == days[j]] * tab3$Close[tab3$Yday == days[j]]
       if (flag == 1) {
          vec3 <- vec1 - kefs[2]*vec2 - kefs[1]
       }
       else  {
        #### vec3 <- vec1 - kefs[2]*vec2 - kefs[1] - vec1[1] + vec2[1]
          vec3 <- vec1 - kefs*vec2
print(summary(vec3))
         #### print(vec1[1] - vec2[1])
       }

ModelDraw(vec1, vec2, "lmweek", kefs, 0, days[j])

       xres <- c(xres, rep(days[j], length(vec3)))
       yres <- c(yres, vec3)
       zres <- c(zres, days[j])
       zzres <- c(zzres, max(abs(vec3)))
#####       if (j>10) {break}
    }
  list(xres = xres, yres = yres, zres = zres,  zzres = zzres)
}

GetErrorsWeek <- function(tab1, tab2, tab3, skip, flag) {
  K1 <- 0.02
  K2 <- 1

  days <- unique(tab1$Yday)
  days <- days[order(days)]
  xres <- c()
  yres <- c()
  zres <- c()
  zzres <- c()  
  mykefs <- rbind(c(), c())
  for (j in ((skip + 1):length(days))) {
    tab1red <- tab1[tab1$Yday %in% days[(j-skip):(j-1)], ]
    tab2red <- tab2[tab2$Yday %in% days[(j-skip):(j-1)], ]
    tab3red <- tab3[tab3$Yday %in% days[(j-skip):(j-1)], ]

print(days[(j-skip):(j-1)])
print(summary(tab1red))

    tab1red$Yday <- 0
    tab2red$Yday <- 0
    tab3red$Yday <- 0
    if (flag==1)  {
      kefs <- GetModelByDay(tab1red, tab2red, tab3red, 0, 1)$coefficients}
    else if (flag<0) {
      vec1 <- tab1red$Close
      vec2 <- tab2red$Close*K1
      vec4 <- tab3red$Close*K2
      kefs <- lm(vec1 ~ vec2 + vec4 + 0)$coefficients  
    }
    else {
      vec1 <- tab1red$Close
      vec2 <- tab2red$Close * tab3red$Close
      vec4 <- tab3red$Close
      kefs <- lm(vec1 ~ vec2 + vec4 + 0)$coefficients 
      ###print(kefs)  
    }
    mykefs <- rbind(mykefs, kefs)
    vec1 <- tab1$Close[tab1$Yday == days[j]]
print(summary(vec1))
    vec2 <- tab2$Close[tab2$Yday == days[j]] * tab3$Close[tab3$Yday == days[j]]
    vec4 <- tab3$Close[tab3$Yday == days[j]]
    if (flag==1)  {
      vec3 <- vec1 - kefs[2]*vec2 - kefs[1]
    }
    else if (flag<0) {
      vec2 <- tab2$Close[tab2$Yday == days[j]]   
      vec3 <- vec1 - kefs[1]*vec2*K1 - kefs[2]*vec4*K2
    }
    else {
      vec3 <- vec1 - kefs[1]*vec2 - kefs[2]*vec4            
    }

     ModelDraw(vec1, vec2, "lmweek", kefs, 1, days[j])

    xres <- c(xres, rep(days[j], length(vec3)))
    yres <- c(yres, vec3)
    zres <- c(zres, days[j])
    zzres <- c(zzres, max(abs(vec3)))        
  }
list(xres = xres, yres = yres, zres = zres, zzres = zzres, mykefs=mykefs)  
} 

GetErrorsAlt <- function(tab1, tab2, tab3, skip) {
  days <- unique(tab1$Yday)
  days <- days[order(days)]
  xres <- c()
  yres <- c()
  zres <- c()
  zzres <- c()  
  for (j in ((skip+1):length(days)))  {
    vec1prev <- tab1$Close[tab1$Yday %in% days[(j-skip):(j-1)]]
    vec2prev <- tab2$Close[tab2$Yday %in% days[(j-skip):(j-1)]] * tab3$Close[tab3$Yday %in% days[(j-skip):(j-1)]]
    vec3prev <- tab2$Close[tab2$Yday %in% days[(j-skip):(j-1)]]
    kefs <- lm(vec1prev ~ vec2prev + vec3prev)$coefficients
print(kefs)
####    kefs <- lm(vec1prev ~ vec2prev + 0)$coefficients
    vec1 <- tab1$Close[tab1$Yday == days[j]]
    vec2 <- tab2$Close[tab2$Yday == days[j]] * tab3$Close[tab3$Yday == days[j]]
    vec3 <- tab2$Close[tab2$Yday == days[j]]
    vec4 <- vec1 - kefs[1] - kefs[2]*vec2 - kefs[3]*vec3

    xres <- c(xres, rep(days[j], length(vec4)))
    yres <- c(yres, vec4)
    zres <- c(zres, days[j])
    zzres <- c(zzres, max(abs(vec4)))        
  }
  list(xres = xres, yres = yres, zres = zres, zzres = zzres)  
}
