KK <- 0.00002
####эта версия работает неправильно
####пользуемся версией из anti_strategy_MMVBArbitrage.R
MMVBArbstrategyI <- function(tabMMVB, tabRTS, tabSI, ind, mask, params, spreads, slippage, candletype="1s", info = INFO) {

 # tabMMVB$Close <- (tabMMVB$Bid + tabMMVB$Ask)/2
#  tabMMVB$Bid <- NULL
#  tabMMVB$Ask <- NULL

  K <- KK
  maxpos <- params$maxpos
  options(stringsAsFactors=FALSE)
  trades <- create.trades()
  curpos <- 0
  curposRTS <- 0
  curposSI <- 0

  if (is.null(tabMMVB$Bid)) {
    tabMMVB$Bid <- tabMMVB$Close - spreads$MMVB
    tabMMVB$Ask <- tabMMVB$Close + spreads$MMVB
  }
  if (is.null(tabRTS$Bid)) {
    tabRTS$Bid <- tabRTS$Close - spreads$RTS
    tabRTS$Ask <- tabRTS$Close + spreads$RTS
  }
  if (is.null(tabSI$Bid)) {
    tabSI$Bid <- tabSI$Close - spreads$SI
    tabSI$Ask <- tabSI$Close + spreads$SI
  }

  j <- 1
  modshiftprev <- 0
  modshift <- 0

  deltapos <- 0
  changeflag <- FALSE

  rr <- which(tabMMVB$Time == min(tabMMVB$Time[tabMMVB$Time >= ind$time[1]]))

  if (params$mode == 1) {
    hedgetime <- tabMMVB$Time[1]
  }

  for (r in (rr+1):nrow(tabMMVB)) {
    if (mask[r] > 0) {
      candle <- tabMMVB[r, ]
      #print(candle$Time)
      ###определение модели
      if (j < nrow(ind)) {
        if (ind$time[j + 1] <= candle$Time) {
          j <- j + 1
          changeflag <- TRUE
        }
      }
      if (candletype == "1s") {
        candleRTSprev <- tabRTS[r-1, ]
        candleSIprev <- tabSI[r-1, ]
      } else if (candletype == "1m") {
        candleRTSprev <- tabRTS[r, ]
        candleSIprev <- tabSI[r, ]
      }
      candleRTS <- tabRTS[r, ]
      candleSI <- tabSI[r, ]
      alpha <- ind$alpha[j]
      beta <- ind$beta[j]

      if (is.na(alpha)) {
        alpha <- 0
      } else if (alpha < 0) {
        alpha <- 0
      }

      model <- beta + alpha * K * ((candleRTSprev$Bid + candleRTSprev$Ask)/2) * ((candleSIprev$Bid + candleSIprev$Ask)/2)

      ###рехеджирование
      if (changeflag) {
        if (curposRTS > alpha * K * curpos) {
          trades <- rbind(trades, sell(candleRTS, volume = curposRTS - alpha * K * curpos, comment = "RTS-rehedge",
                                       slippage = slippage$RI, instrument = "RTS"))
          curposRTS <- alpha * K * curpos
          vol <- (curposRTS - alpha * K * curpos) * candleRTS$Bid *
            info$point.price[info$instrument == "RI"]/info$point.price[info$instrument == "SI"]
          trades <- rbind(trades, sell(candleSI, volume = vol, comment = "SI",
                                       slippage = slippage$SI, instrument = "SI"))
          curposSI <- curposSI - vol
        }
        else if (curposRTS < alpha * K * curpos) {
          trades <- rbind(trades, buy(candleRTS, volume = -curposRTS + alpha * K * curpos, comment = "RTS-rehedge",
                                      slippage = slippage$RI, instrument = "RTS"))
          curposRTS <- alpha * K * curpos
          vol <- (-curposRTS + alpha * K * curpos) * candleRTS$Ask *
            info$point.price[info$instrument == "RI"]/info$point.price[info$instrument == "SI"]
          trades <- rbind(trades, buy(candleSI, volume = vol, comment = "SI",
                                      slippage = slippage$SI, instrument = "SI"))
          curposSI <- curposSI + vol
        }
      }
      changeflag <- FALSE


      #print(c(alpha, beta, model, (candle$Bid + candle$Ask)/2))
      ###проверка возможности входа
      gap <- ((candle$Bid + candle$Ask)/2 - model - modshiftprev) %/% params$size
      #print(gap)

      if (gap > 0) {
        for (i in 1:(gap+1)) {
          if (params$MODE == 1) {
            if (candle$Bid >= model + modshiftprev + i*params$size) {
              if (curpos - params$multip[i] >= -maxpos) {
                modshift <- modshift + params$shift
                trades <- rbind(trades, sell(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                                             slippage = slippage$MIX,
                                             limit.price = model + i*params$size + modshiftprev, instrument = "MIX"))
                curpos <- curpos - params$multip[i]
                deltapos <- deltapos - params$multip[i]
              } else {
                break
              }
            } else {
              break
            }
          } else if (params$MODE == 0) {
            candleMean <- (candle$Bid + candle$Ask)/2
            if (candleMean - spreads$MMVB >= model + i*params$size + modshiftprev) {
              if (curpos + params$multip[i] >= -maxpos) {
                modshift <- modshift + params$shift
                trades <- rbind(trades, sell(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                                                                                             slippage = slippage$MIX, instrument = "MIX"))
                curpos <- curpos - params$multip[i]
                deltapos <- deltapos - params$multip[i]
              } else {
                break
              }
            } else {
              break
            }
          }
        }
        modshiftprev <- modshift
      }
      else if (gap < 0) {
        gap <- -gap
        for (i in 1:(gap)) {
          if (params$MODE == 1) {
             if (candle$Ask <= model - i*params$size + modshiftprev) {
                if (curpos + params$multip[i] <= maxpos) {
                 modshift <- modshift - params$shift
                  trades <- rbind(trades, buy(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                                           slippage = slippage$MIX,
                                           limit.price = model - i*params$size + modshiftprev, instrument = "MIX"))
                curpos <- curpos + params$multip[i]
                deltapos <- deltapos + params$multip[i]
             } else {
               break
             }
           } else {
            break
           }
          } else if (params$MODE == 0) {
             candleMean <- (candle$Bid + candle$Ask)/2
             if (candleMean + spreads$MMVB <= model - i*params$size + modshiftprev) {
               if (curpos + params$multip[i] <= maxpos) {
                 modshift <- modshift - params$shift
                 trades <- rbind(trades, buy(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                                          slippage = slippage$MIX, instrument = "MIX"))
                 curpos <- curpos + params$multip[i]
                 deltapos <- deltapos + params$multip[i]
               } else {
                break
              }
            } else {
              break
            }
          }
        }
        modshiftprev <- modshift
      } else {
        deltapos <- 0
      }
      ###хеджирование
      if (deltapos > 0) {
 #       print("RTS true spread1")
#        print((candleRTS$Ask - candleRTS$Bid)/2)
#        print(candleRTS)
        trades <- rbind(trades, sell(candleRTS, volume = alpha*abs(deltapos)*K, comment = "RTS", slippage = slippage$RI,
                                     instrument = "RI"))
        curposRTS <- curposRTS - abs(deltapos) * alpha * K
 #       print("SI true spread")
#        print((candleSI$Ask - candleSI$Bid)/2)
        vol <- alpha*abs(deltapos)*candleRTS$Bid*K*
                  info$point.price[info$instrument == "RI"]/info$point.price[info$instrument == "SI"]
        trades <- rbind(trades, sell(candleSI, volume = vol, comment = "SI",
                                     slippage = slippage$SI, instrument = "SI"))
        curposSI <- curposSI - vol
        deltapos <- 0
      } else if (deltapos < 0) {
 #       print("RTS true spread2")
#        print((candleRTS$Ask - candleRTS$Bid)/2)
        trades <- rbind(trades, buy(candleRTS, volume = alpha*abs(deltapos) *K, comment = "RTS", slippage = slippage$RI,
                                    instrument = "RI"))
        curposRTS <- curposRTS + abs(deltapos) * alpha * K
 #       print("SI true spread")
#        print((candleSI$Ask - candleSI$Bid)/2)
        vol <- alpha*abs(deltapos)*candleRTS$Ask * K*
                  info$point.price[info$instrument == "RI"]/info$point.price[info$instrument == "SI"]
        trades <- rbind(trades, buy(candleSI, volume = vol, comment = "SI",
                                    slippage = slippage$SI, instrument = "SI"))
        curposSI <- curposSI + vol
        deltapos <- 0
      }

      if (params$mode == 1) {
        ###рехеджирование по времени
        if (as.numeric(candle$Time - hedgetime, units="secs") >= params$deltatime) {
          redeltapos <- GetDeltaPos(curposRTS, curposSI, candleRTS, candleSI, info)$deltapos
          if (redeltapos < -0.1) {
           trades <- rbind(trades, sell(candleSI,
                                        volume = abs(redeltapos)/info$point.price[info$instrument == "SI"], comment = "SI-rehedge",
                                         slippage = slippage$SI, instrument = "SI"))
            curposSI <- curposSI - abs(redeltapos)
          } else if (redeltapos > 0.1) {
            trades <- rbind(trades, buy(candleSI, volume = abs(redeltapos)/info$point.price[info$instrument == "SI"], comment = "SI-rehedge",
                                        slippage = slippage$SI, instrument = "SI"))
            curposSI <- curposSI + abs(redeltapos)
          }
          hedgetime <- candle$Time
        }
      } else if (params$mode == 2) {
        ###рехеджирование относительное
        temp <- GetDeltaPos(curposRTS, curposSI, candleRTS, candleSI, info)
        redeltapos <- temp$deltapos
        truepos <- temp$pos
        if (abs(redeltapos/truepos) > params$critval) {
          if (redeltapos < 0) {
            trades <- rbind(trades, sell(candleSI, volume = abs(redeltapos)/info$point.price[info$instrument == "SI"], comment = "SI-rehedge",
                                         slippage = slippage$SI, instrument = "SI"))
            curposSI <- curposSI - abs(redeltapos)
          } else if (redeltapos > 0) {
            trades <- rbind(trades, buy(candleSI, volume = abs(redeltapos)/info$point.price[info$instrument == "SI"], comment = "SI-rehedge",
                                        slippage = slippage$SI, instrument = "SI"))
            curposSI <- curposSI + abs(redeltapos)
          }
        }
      }
      if (mask[r] == 2) {
        trades <- rbind(trades, close.position(candle, curpos, slippage$MIX, instrument = "MIX"))
        curpos <- 0
        trades <- rbind(trades, close.position(candleRTS, curposRTS, slippage$RI, instrument = "RI"))
        curposRTS <- 0
        trades <- rbind(trades, close.position(candleSI, curposSI, slippage$SI, instrument = "SI"))
        curposSI <- 0
      }
    }
  }
  trades$time <- as.POSIXct(trades$time, origin = "1970-01-01", tz = "UTC")
  if (!params$printmode) {
    res <- trades
  } else {
    res <- list(trades = trades, printinfo = data.frame(date = dateeff, model = modeleff, cash = casheff, mmvb = mmvbeff))
  }
res
}


GetDeltaPos <- function(curposRTS, curposSI, candleRTS, candleSI, stepprice) {
  posRTS <- TruePos(curposRTS, candleRTS, point.price = stepprice)
  posSI <- curposSI
  modelpos <- posRTS - posSI
list(pos = posRTS + 0.00001, deltapos = modelpos)
}

TruePos <- function(curpos, candle, point.price = 1) {
  if (curpos>0) {
    res <- curpos * candle$Bid * point.price
  } else if (curpos < 0) {
    res <- curpos * candle$Ask * point.price
  } else {
    res <- 0
  }
res
}
