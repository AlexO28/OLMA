Formatize <- function(tab) {
  times <- strptime(paste(tab$X.DATE., tab$X.TIME), format = "%Y%m%d %H:%M:%S")
  closes <- tab$X.CLOSE.
data.frame(time = times, close = closes)
}

InnerMergeDf <- function(list.df){
	# each df has Time as key-value
	# return merged df
	df.start <- list.df[[1]]

	if (is.null(df.start$time)) {
	  stop("data.frame N1 does not have time column!")
	}        

	if (length(list.df) > 1) {
	  for (i in 2:length(list.df)) {
	    df.next <- list.df[[i]]
	    if (is.null(df.start$time)) {
	    	stop(paste0("data.frame ", i, " does not have time column!"))
	    }
	    ind.start <- df.start$time %in% df.next$time
	    ind.next <- df.next$time %in% df.start$time
	    df.start <- cbind(df.start[ind.start, ], df.next[ind.next, ])
	  }
	}

        i <- 1
        j <- 1
        while (j<=ncol(df.start)) {
          print(c(i, j))
          df.next <- list.df[[i]]
          for (k in 1:ncol(df.next)) {
            print(k)
            names(df.start)[j] <- paste0(names(list.df[i]), ".", names(df.next)[k])
            j <- j + 1
          }
          i <- i + 1
        }

	df.start
}
GetAlphaByDay <- function(day, tab, num) {
  days <- unique(tab$date)
  daypos <- which(days == day)
  tab2 <- tab[(tab$date <= as.Date(days[daypos - 1])) & (tab$date >= as.Date(days[daypos - num])), ]
as.numeric(coef(lm(tab2$mmvb ~ I(tab2$rts * 0.00002 * tab2$dol) + 0)))
}
GetAlphas <- function(tab, num) {
  days <- unique(tab$date)
  days <- as.Date(days[(num+1):length(days)])
list(days = days, alphas = sapply(days, FUN = GetAlphaByDay, tab = tab, num = num))
}
FillDaysTillExpir <- function(tab, startdate, enddate, num) {
  tab$daystillexpir[tab$date > startdate & tab$date < enddate] <- 
      as.numeric(enddate - 
         tab$date[tab$date > startdate & tab$date < enddate])
   tab$periodnum[tab$date > startdate & tab$date < enddate] <- num
tab
}
Initialization <- function(tab, num) {
  tab <- FillDaysTillExpir(tab, as.Date("2013-12-16"), as.Date("2014-03-17"), num)
  tab <- FillDaysTillExpir(tab, as.Date("2014-03-17"), as.Date("2014-06-16"), num+1)
  tab <- FillDaysTillExpir(tab, as.Date("2014-06-16"), as.Date("2014-09-15"), num+2)
  tab <- FillDaysTillExpir(tab, as.Date("2014-09-15"), as.Date("2014-12-15"), num+3)
  tab <- FillDaysTillExpir(tab, as.Date("2014-12-15"), as.Date("2015-03-16"), num+4)
  tab <- FillDaysTillExpir(tab, as.Date("2015-03-16"), as.Date("2015-06-15"), num+5)
  tab <- FillDaysTillExpir(tab, as.Date("2015-06-15"), as.Date("2015-09-15"), num+6)
tab
}
StavkaStressTest <- function(date0, tab, silent = FALSE) {
  print(date0)
  tab <- tab[tab$date == date0, ]
  stavkammvb <- (tab$mmvbfut/100 - tab$mmvbind)
  stavkarts <- (tab$rtsfut/100 - tab$rtsind)
  stavkadol <- (tab$dolfut/1000 - tab$dol)
  annstavkammvb <- stavkammvb/tab$daystillexpir
  annstavkarts <- stavkarts/tab$daystillexpir
  annstavkadol <- stavkadol/tab$daystillexpir
  if (silent == FALSE) {
    print("stavka")
    print(summary(cbind(stavkammvb, stavkarts, stavkadol)))
    print("annualized stavka")
    print(summary(cbind(annstavkammvb, annstavkarts, annstavkadol)))
  }
  kefsprev <- coef(lm(tab$mmvbfut ~ I(0.00002 * tab$rtsfut * tab$dolfut)))
  kefsprev0 <- coef(lm(tab$mmvbfut ~ I(0.00002 * tab$rtsfut * tab$dolfut) + 0))
  if (silent == FALSE) {
    print("coefficients for current situtation")
    print(kefsprev)
    print(kefsprev0)
  }
  annstavkammvbnew <- 2*annstavkammvb
  annstavkartsnew <- 2*annstavkarts
  annstavkadolnew <- 2*annstavkadol
  if (silent == FALSE) {
    print("new annualized stavka")
    print(summary(cbind(annstavkammvbnew, annstavkartsnew, annstavkadolnew)))
  }
  stavkammvbnew <- annstavkammvbnew*tab$daystillexpir
  stavkartsnew <- annstavkartsnew*tab$daystillexpir
  stavkadolnew <- annstavkadolnew*tab$daystillexpir
  if (silent == FALSE) {
    print("new stavka")
    print(summary(cbind(stavkammvbnew, stavkartsnew, stavkadolnew)))
  }
  tab$mmvbfut <- tab$mmvbfut + 100*(stavkammvbnew - stavkammvb)/2
  tab$rtsfut <- tab$rtsfut + 100*(stavkartsnew - stavkarts)/2
  tab$dolfut <- tab$dolfut + 1000*(stavkadolnew - stavkadol)/2
  kefs <- coef(lm(tab$mmvbfut ~ I(0.00002 * tab$rtsfut * tab$dolfut)))
  kefs0 <- coef(lm(tab$mmvbfut ~ I(0.00002 * tab$rtsfut * tab$dolfut) + 0))
  if (silent == FALSE) {
    print("stress test")
    print(kefs)
    print(kefs0)
  }
as.numeric(c(kefsprev, kefsprev0, kefs, kefs0, tab$daystillexpir[1]))
}
StavkaStressTestMain <- function(tab) {
  dates <- unique(tab$date)
t(sapply(dates, FUN = "StavkaStressTest", tab = tab, silent = TRUE))
}
###cor is really a much better idea!
MyCorTest <- function(largetab, indices) {
  res <- data.frame(i = character(), j = character(), pvalue = numeric(), left = numeric(), right = numeric())
  for (i in indices) {
    for (j in indices) {
      temp <- cor.test(largetab[, i], largetab[, j])
      confint <- as.numeric(temp$conf.int)
      res <- rbind(res, data.frame(i = names(largetab)[i], j = names(largetab)[j], pvalue = temp$p.value, left = confint[1], right = confint[2])) 
    }
  }
res
}
KillExtraCols <- function(tab, num) {
  tab2 <- tab
  for (j in seq(1, num, 2)) {
    tab2[, names(tab)[j]] <- NULL
  }
tab2
}
MainInitialization <- function(tab, num) {
  tab$time <- tab$mix.time
  tab$date <- as.Date(tab$time)
  tab <- Initialization(tab, 1)
  tab <- tab[tab$daystillexpir > 5 & tab$daystillexpir < 85, ]
na.omit(KillExtraCols(tab, num))
}
GetResidualsForVarModel <- function(num, tab, control = 1) {
  len <- length(unique(tab$date))
  if (control == 1) {
    reskefs <- matrix(NA, len, 6)
  } else if (control == 2) {
    reskefs <- matrix(NA, len, 5)
  } else if (control == 3) {
    reskefs <- matrix(NA, len, 4)
  } else if (control == 4) {
    reskefs <- matrix(NA, len, 9)
  }
  resresid <- data.frame(date = rep(as.Date("2010-11-12"), nrow(tab)), resid = rep(NA, nrow(tab)), gaps = rep(NA, nrow(tab)))
  counter <- 1
  kefsprev <- NA
  #for (periodnum in unique(tab$periodnum)) {
  #  tabforper <- tab[tab$periodnum == periodnum, ]
    dates <- unique(tab$date)
    for (j in 2:length(dates)) {
      print(counter)
      print(paste(as.Date(dates[j])))
      tabred <- tab[tab$date >= dates[max(1, j-num)] & tab$date <= dates[j-1], ]
      if (control == 1) {
        reg <- lm(tabred$mix.close ~ tabred$gazr.close + tabred$lkoh.close + tabred$sbrf.close)
      } else if (control == 2) {
        reg <- lm(tabred$mix.close ~ tabred$gazr.close + tabred$lkoh.close)
      } else if (control == 3) {
        reg <- lm(tabred$mix.close ~ tabred$gazr.close)
      } else if (control == 4) {
        reg <- lm(tabred$mix.close ~ tabred$gazr.close + tabred$lkoh.close + tabred$sbrf.close + tabred$gmkr.close + tabred$vtbr.close + tabred$rosn.close)
      }
      kefs <- c(as.numeric(coef(reg)), summary(reg)$adj.r.squared, as.Date(dates[j]))  
    #  print(kefs)
      reskefs[j, ] <- kefs
      tabred <- tab[tab$date == dates[j], ]
     # if (control == 1) {
     #   resid <- tabred$mix.close - (kefs[1] + kefs[2]*tabred$gazr.close + kefs[3]*tabred$lkoh.close + kefs[4]*tabred$sbrf.close)
     # } else if (control == 2) {
     #   resid <- tabred$mix.close - (kefs[1] + kefs[2]*tabred$gazr.close + kefs[3]*tabred$lkoh.close)
     # } else if (control == 3) {
     #   resid <- tabred$mix.close - (kefs[1] + kefs[2]*tabred$gazr.close)
     # } else if (control == 4) {
     #   resid <- tabred$mix.close - (kefs[1] + kefs[2]*tabred$gazr.close + kefs[3]*tabred$lkoh.close + 
     #              kefs[4]*tabred$sbrf.close + kefs[5]*tabred$gmkr.close + kefs[6]*tabred$vtbr.close + kefs[7]*tabred$rosn.close)
     # }
     # if (is.na(kefsprev)) {
     #   val <- 0
     # } else {
     #   if (control == 1) {
     #     val <- kefs[1] - kefsprev[1] + (kefs[2] - kefsprev[2])*tabred$gazr.close[1] + (kefs[3] - kefsprev[3])*tabred$lkoh.close[1] +
     #                 (kefs[4] - kefsprev[4])*tabred$sbrf.close[1]
     #   } else if (control == 2) {
     #     val <- kefs[1] - kefsprev[1] + (kefs[2] - kefsprev[2])*tabred$gazr.close[1] + (kefs[3] - kefsprev[3])*tabred$lkoh.close[1]
     #   } else if (control == 3) {
     #     val <- kefs[1] - kefsprev[1] + (kefs[2] - kefsprev[2])*tabred$gazr.close[1]
      #  } else if (control == 4) {
      #    val <- kefs[1] - kefsprev[1] + (kefs[2] - kefsprev[2])*tabred$gazr.close[1] + (kefs[3] - kefsprev[3])*tabred$lkoh.close[1] +
      #                (kefs[4] - kefsprev[4])*tabred$sbrf.close[1] + (kefs[5] - kefsprev[5])*tabred$gmkr.close + 
       #               (kefs[6] - kefsprev[6])*tabred$vtbr.close + (kefs[7] - kefsprev[7])*tabred$rosn.close
      #  }
      #}
      gc()
      for (k in 1:nrow(tabred)) {
        resresid$date[counter] <- dates[j]
        if (control ==  4) {
          resresid$resid[counter] <- tabred$mix.close[k] - (kefs[1] + kefs[2]*tabred$gazr.close[k] + kefs[3]*tabred$lkoh.close[k] + 
                   kefs[4]*tabred$sbrf.close[k] + kefs[5]*tabred$gmkr.close[k] + kefs[6]*tabred$vtbr.close[k] + kefs[7]*tabred$rosn.close[k])
          resresid$gaps[counter] <- kefs[1] - kefsprev[1] + (kefs[2] - kefsprev[2])*tabred$gazr.close[1] + (kefs[3] - kefsprev[3])*tabred$lkoh.close[1] +
                      (kefs[4] - kefsprev[4])*tabred$sbrf.close[1] + (kefs[5] - kefsprev[5])*tabred$gmkr.close + 
                      (kefs[6] - kefsprev[6])*tabred$vtbr.close + (kefs[7] - kefsprev[7])*tabred$rosn.close
          counter <- counter + 1
        } else if (control == 1) {
          resresid$resid[counter] <- tabred$mix.close[k] - (kefs[1] + kefs[2]*tabred$gazr.close[k] + kefs[3]*tabred$lkoh.close[k] + 
                   kefs[4]*tabred$sbrf.close[k])
          resresid$gaps[counter] <- kefs[1] - kefsprev[1] + (kefs[2] - kefsprev[2])*tabred$gazr.close[1] + (kefs[3] - kefsprev[3])*tabred$lkoh.close[1] +
                      (kefs[4] - kefsprev[4])*tabred$sbrf.close[1]
          counter <- counter + 1
        }
      }    
     kefsprev <- kefs
      #resresid <- rbind(resresid, data.frame(date = (rep(dates[j], nrow(tabred))), resid = resid, gaps = rep(val, nrow(tabred))))
    #  break
   # }
   # break
  }
list(reskefs = na.omit(reskefs), resresid = na.omit(resresid))
}
DrawTriplePicture <- function(tab, name) {
  par(mfcol = c(3,1))
  boxplot(tab$resid ~ tab$date, ylab = "residuals")
  abline(h = 0, col = "red")
  boxplot(tab$gaps ~ tab$date, ylab = "gaps") 
  abline(h = 300, col = "red")
  abline(h = -300, col = "red")
  boxplot(tab[, name] ~ tab$date, ylab = paste("stavka of ", name), ylim = c(-10, 50))
}
CalculateResidualsForStavka <- function(tab) {
  resmat <- matrix(NA, nrow(tab), 2)
  dates <- unique(tab$date[is.na(tab$kefs1) == FALSE])
  counter <- 1
  for (date in dates) {
    print(as.Date(date, origin = "1970-01-01"))
    tabred <- tab[tab$date == date, ]
    for (k in 1:nrow(tabred)) {
      resmat[counter, 1] <- 
          tabred$mix.stavka[k] - tabred$kefs1[k]/100 - tabred$kefs2[k]*tabred$gazr.stavka[k] - 
               tabred$kefs3[k]*tabred$lkoh.stavka[k]/10 - tabred$kefs4[k]*tabred$sbrf.stavka[k]
      resmat[counter, 2] <- date
      counter <- counter + 1
    }
  }
na.omit(resmat)
}
MMVBModel1Draw <- function(tab) {
  par(mfcol = c(3, 1))
  boxplot(tab$residvec ~ tab$date, ylab = "model residuals")
  abline(h = 0, col = "red")
  boxplot(tab$annstavka1 ~ tab$date, ylim = c(-10, 15), ylab = "MMVBStavka")
  abline(h = median(tab$annstavka1), col = "red")
  boxplot(tab$annstavka2 ~ tab$date, ylim = c(-20, 10), ylab = "RTSStavka")
  abline(h = median(tab$annstavka2), col = "red")
}
MMVBModel2Draw <- function(largetab, restab) {
  par(mfcol = c(4, 1))
  medtab1 <- aggregate(resid ~ date, data = restab, FUN = median)
  boxplot(medtab1$resid ~ medtab1$date, type = "b", ylab = "model residuals")
#  boxplot(restab$resid ~ restab$date, ylab = "model residuals")
  abline(h = 0, col = "red")
  medtab2 <- aggregate(annstavka1 ~ date, data = largetab, FUN  = median)
  boxplot(medtab2$annstavka1 ~ medtab2$date, type = "b", ylab = "MMVBStavka", ylim = c(-10, 15))
#  boxplot(largetab$annstavka1 ~ largetab$date, ylim = c(-10, 15), ylab = "MMVBStavka")
  abline(h = median(largetab$annstavka1), col = "red")
  medtab3 <- aggregate(annstavka2 ~ date, data = largetab, FUN  = median)
  boxplot(medtab3$annstavka2 ~ medtab3$date, type = "b", ylab = "RTSStavka", ylim = c(-20, 10))
#  boxplot(largetab$annstavka2 ~ largetab$date, ylim = c(-20, 10), ylab = "RTSStavka")
  abline(h = median(largetab$annstavka2), col = "red") 
  boxplot(medtab2$annstavka1 - medtab3$annstavka2 ~ medtab2$date, ylab = "Difference of MMVBStavka and RTSStavka", ylim = c(-5, 15))
  abline(h = median(largetab$annstavka1) - median(largetab$annstavka2), col = "red")
list(medtab1, medtab2, medtab3)
}

MMVBModel3Draw <- function(tabfut, tabind) {
  par(mfcol = c(2, 1))
  boxplot(tabfut$model ~ tabfut$date, ylab = "futures model", ylim = c(-500, 500))
  for (j in unique(tabfut$periodnum)) {
    tabred <- tabfut[tabfut$periodnum == j, ]
    datemin <- tabred$date[which.min(tabred$daystillexpir)]
print(datemin)
    abline(v = as.Date(datemin), col = "red")
  }
  boxplot(tabind$model ~ tabind$date,  ylab = "spot model", ylim = c(-500, 500))
  for (j in unique(tabind$periodnum)) {
    tabred <- tabind[tabind$periodnum == j, ]
    datemin <- tabred$date[which.min(tabred$daystillexpir)]
    abline(v = as.Date(datemin), col = "red")
  }
}

FillModel <- function(tab, alpha, beta) {
  tab$model <- tab$mix.close - alpha*0.00002*tab$rts.close*tab$si.close - beta
  tab$modelind <- tab$mixind.close*100 - alpha*2*tab$rtsind.close*tab$siind.close - beta
tab
}
ClassifyByIndCrit <- function(tab, eps, signum = 1, thename = "modelind") {
  crit <- numeric(nrow(tab))
  mid <- median(tab[, thename])
  if (signum > 0) {
    crit <- ifelse(tab[, thename] - mid >= eps, 1, -1)  
  } else {
    crit <- ifelse(tab[, thename] - mid < -eps, 1, -1)
  }
data.frame(crit = crit, id = 1:length(crit))
}
ClassifyByIndCritUp <- function(tab, eps1, eps2, signum = 1, thename = "modelind") {
  crit <- numeric(nrow(tab))
 ### mid1 <- median(tab$model)
  mid1 <- 0
  mid2 <- median(tab[, thename])
  print(c(mid1, mid2))
  if (signum > 0) {
    crit <- ifelse((tab[, thename] - mid2 >= eps2) & (tab$model - mid1 >= eps1), 1, -1)
  } else {
    crit <- ifelse((tab[, thename] - mid2 < -eps2) & (tab$model - mid1 < -eps1), 1, -1)
  }
data.frame(crit = crit, id = 1:length(crit))
}
CheckAbstractCrit <- function(tab, crit, step = 60, signum = 1) {
  crit <- crit[crit$crit == signum, ]
  sapply(crit[, 2], FUN = "CheckAbstractCritServe", tab = tab, step = step, signum = signum)
}
CheckAbstractCritServe <- function(crit, tab, step, signum) {
  curval <- tab$model[crit]
  nextval <- median(tab$model[crit:min(crit + step, nrow(tab))])
  if (signum > 0) {
    if (nextval <= curval) {return(1)} else {return(-1)}
  } else {
    if (nextval >= curval) {return(1)} else {return(-1)}
  }
}
CalculateAnnualizesStavka <- function(tab) {
  tab$stavkamix <- 100*365*(tab$mix.close/100 - tab$mixind.close)/(tab$mixind.close*tab$daystillexpir)
  tab$stavkarts <- 100*365*(tab$rts.close/100 - tab$rtsind.close)/(tab$rtsind.close*tab$daystillexpir)
  tab$stavkasi <- 100*365*(tab$si.close/1000 - tab$siind.close)/(tab$siind.close*tab$daystillexpir)
tab
}