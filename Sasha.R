GetStatistics <- function(tab, n) {
  vec <- tab$Close[seq(1, nrow(tab), n)]
  vec <- diff(vec)
  resdat <- data.frame(step = numeric(), n = numeric(), val = numeric())
  for (astep in seq(-100, 100, 5)) {
    print(astep)
    res <- length(vec[vec == astep])
    resdat <- rbind(resdat, data.frame(step = astep, n = n, val = res))
  }
resdat
}

AddMean <- function(tab, n) {
  tab[, mean := SMA(Close, n)]
}
MakeTable <- function(vec, len) {
  res <- data.frame(minus = numeric(2*len), plus = numeric(2*len))
  counter <- 1
  for (i in seq(0.5, len, 0.5)) {
    print(i)
    val1 <- length(vec[vec >= i*10])
    val2 <- length(vec[vec <= -i*10])
    res[counter, 1] <- val2
    res[counter, 2] <- val1
    print(c(val1, val2))
    counter <- counter + 1
  }
res
}
BigTrendSeeker <- function(tab, narezka, wind, xxx) {
  tab <- as.data.table(as.data.frame(tab))
  tab$mean <- NULL
  tab <- na.omit(AddMean(tab, wind))
  tab <- tab[seq(1, nrow(tab), narezka), ]
  tab3 <- tab
  rm(tab)
  diffs <- diff(tab3$mean)
  trendstat <- 0
  difftab <- data.frame(diffs = diffs, id = 1:length(diffs))
  shortdiffs <- difftab[abs(difftab$diffs) >= 0.5, ]
  print(nrow(shortdiffs))
  restab <- data.frame(trendstat = rep(as.numeric(NA), nrow(shortdiffs)),
                       trenddur = rep(as.numeric(NA), nrow(shortdiffs)),
                       trendsize = rep(as.numeric(NA), nrow(shortdiffs)))
  tab3next <- tab3[shortdiffs$id + 1, ]
  tab3 <- tab3[shortdiffs$id, ]

  shortdiffs$id <- 1:nrow(shortdiffs)

#  print(head(shortdiffs))
#  print(tail(shortdiffs))
#  stop()
#  print(nrow(tab3))
#  print(nrow(tab3next))

  for (ind in 1:nrow(shortdiffs)) {
    if (ind %% 100 == 0) {print(ind)}
#    print(shortdiffs[1, ])
    curpos <- shortdiffs$id[ind]
#    print(curpos)
#    print(tab3[(curpos-1):(curpos+1), ])
    trendstat <- tab3next$mean[curpos] - tab3$mean[curpos]
#    print(tab3$mean[curpos:(curpos+1)])
#    print(trendstat)
    if (abs(trendstat)<0.1){
      stop("Diff problem detected!")
    }
    if (ind + 1 > nrow(shortdiffs)) {
      restab$trendstat[ind] <- sign(trendstat)
      restab$trenddur[ind] <- round(tab3next$TimeNum[curpos] - tab3$TimeNum[curpos], 3)
      restab$trendsize[ind] <- abs(trendstat)
      break
    }
#    found <- FALSE
    for (j in (ind+1):nrow(shortdiffs)) {
      curpos2 <- shortdiffs$id[j]
   #   print(c(curpos, curpos2))
      if (tab3next$TimeNum[curpos2] - tab3$TimeNum[curpos] >= 300) {break}
      if (trendstat > 0) {
        if (tab3next$mean[curpos2] < tab3$mean[curpos2]) {
          ###possible fall
      #    print("possible fall")
          maxval <- max(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:(curpos2)])
          if ((maxval - tab3next$mean[curpos2])/(maxval - tab3$mean[curpos]) > xxx) {
         #   found <- TRUE
            break
          }
        }
      } else {
        if (tab3next$mean[curpos2] > tab3$mean[curpos2]) {
          ###possible increase
      #    print("possible increase")
          minval <- min(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:curpos2])
          if ((tab3next$mean[curpos2] - minval)/(tab3$mean[curpos] - minval) > xxx) {
        #    found <- TRUE
            break
          }
        }
      }
    }
    curpos2 <- shortdiffs$id[j]
    restab$trendstat[ind] <- sign(trendstat)
    restab$trenddur[ind] <- round(tab3next$TimeNum[curpos2] - tab3$TimeNum[curpos], 3)
    restab$trendsize[ind] <- ifelse(trendstat>0, max(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:curpos2]) - tab3$mean[curpos],
                                    tab3$mean[curpos] - min(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:curpos2]))
  }
restab
}
MainBigTrendSeekerTrue <- function(tab) {
  for (narezka in c(10, 300)) {
    for (wind in c(10, 100, 300)) {
      for (xxx in seq(0, 0.9, 0.05)) {
        hres <- na.omit(BigTrendSeekerTrue(tab, narezka, wind, xxx))
        nums <- numeric(10)
        nums2 <- numeric(10)
        for (j in 1:10) {
          nums[j] <- nrow(hres[hres$trendsize >= j*10, ])
          nums2[j] <- median(hres$trenddur[hres$trendsize >= j*10])
        }
        print(nums)
        #hdat <- data.frame(x = narezka, y = wind, z = xxx)
        #hdat <- cbind(hdat, nums)
        #print(hdat)
        write.table(as.data.frame(rbind(c(), c(narezka, wind, xxx, nums, nums2))), "funnyresults3.txt", append = TRUE, sep = ";", col.names = FALSE, row.names = FALSE)
      }
    }
  }
}
BigTrendSeekerTrue <- function(tab, narezka, wind, xxx) {
  tab <- as.data.table(as.data.frame(tab))
  tab$mean <- NULL
  tab <- na.omit(AddMean(tab, wind))
  tab <- tab[seq(1, nrow(tab), narezka), ]
  tab3 <- tab
  print(nrow(tab3))
  rm(tab)
  diffs <- diff(tab3$mean)
  trendstat <- 0
  difftab <- data.frame(diffs = diffs, id = 1:length(diffs))
  print(nrow(difftab))
  print(summary(abs(difftab$diffs)))
  shortdiffs <- difftab[abs(difftab$diffs) >= 0.5, ]
  print(nrow(shortdiffs))
  restab <- data.frame(trendstat = rep(as.numeric(NA), nrow(shortdiffs)),
                       trenddur = rep(as.numeric(NA), nrow(shortdiffs)),
                       trendsize = rep(as.numeric(NA), nrow(shortdiffs)))
  tab3next <- tab3[shortdiffs$id + 1, ]
  tab3 <- tab3[shortdiffs$id, ]

  #return(tab3)

  shortdiffs$id <- 1:nrow(shortdiffs)

  ind <- 1
  while (ind <= nrow(shortdiffs)) {
    print(paste("big", ind))
#    if (ind %% 100 == 0) {print(ind)}
    #    print(shortdiffs[1, ])
    curpos <- shortdiffs$id[ind]
    #    print(curpos)
    #    print(tab3[(curpos-1):(curpos+1), ])
    trendstat <- tab3next$mean[curpos] - tab3$mean[curpos]
    #    print(tab3$mean[curpos:(curpos+1)])
    #    print(trendstat)
    if (abs(trendstat)<0.1){
      stop("Diff problem detected!")
    }
    if (ind + 1 > nrow(shortdiffs)) {
      restab$trendstat[ind] <- sign(trendstat)
      restab$trenddur[ind] <- round(tab3next$TimeNum[curpos] - tab3$TimeNum[curpos], 3)

      extremval <- ifelse(trendstat>0, max(tab3$mean[curpos:(nrow(tab3))], tab3next$mean[curpos:(nrow(tab3))]),
                          min(tab3$mean[curpos:(nrow(tab3))], tab3next$mean[curpos:(nrow(tab3))]))

      restab$trendsize[ind] <- ifelse(trendstat>0, extremval - tab3$mean[curpos],
                                      tab3$mean[curpos] - extremval)
      break
    }
    #    found <- FALSE
    extremval <- NA
    for (j in (ind+1):nrow(shortdiffs)) {
      curpos2 <- shortdiffs$id[j]
        print(paste("small", j))
      #  print(trendstat)
   #   if (tab3$TimeNum[curpos2+1] - tab3$TimeNum[curpos] >= 300) {break}
      if (trendstat > 0) {
        if (tab3next$mean[curpos2] < tab3$mean[curpos2]) {
          ###possible fall
           #   print("possible fall")
       #   print((maxval - tab3next$mean[curpos2])/(maxval - tab3$mean[curpos]))
          maxval <- max(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:(curpos2)])
          extremval <- maxval
          if ((maxval - tab3next$mean[curpos2])/(maxval - tab3$mean[curpos]) > xxx) {
            #   found <- TRUE
            break
          }
        }
      } else {
        if (tab3next$mean[curpos2] > tab3$mean[curpos2]) {
          ###possible increase
            #  print("possible increase")
          minval <- min(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:curpos2])
          extremval <- minval
       #   print(paste(minval, tab3$mean[curpos], tab3next$mean[curpos2], tab3$Time[curpos2], tab3$Time[curpos]))
        #  print((tab3next$mean[curpos2] - minval)/(tab3$mean[curpos] - minval))

          if ((tab3next$mean[curpos2] - minval)/(tab3$mean[curpos] - minval) > xxx) {
            #    found <- TRUE
            break
          }
        }
      }
    }
    curpos2 <- shortdiffs$id[j]
    restab$trendstat[ind] <- sign(trendstat)
    restab$trenddur[ind] <- round(tab3next$TimeNum[curpos2] - tab3$TimeNum[curpos], 3)
   # restab$trendsize[ind] <- ifelse(trendstat>0, max(tab3$mean[curpos:(curpos2+1)]) - tab3$mean[curpos],
  #                                  tab3$mean[curpos] - min(tab3$mean[curpos:(curpos2+1)]))
    if (is.na(extremval)) {
      extremval <- ifelse(trendstat>0, max(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:(curpos2)]),
                                                          min(tab3$mean[curpos:(curpos2)], tab3next$mean[curpos:(curpos2)]))
    }
     restab$trendsize[ind] <- ifelse(trendstat>0, extremval - tab3$mean[curpos],
                                      tab3$mean[curpos] - extremval)
    ind <- j + 1
   # if (ind>30) {break}
  }
  restab
}
