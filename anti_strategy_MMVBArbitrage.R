KK <- 0.00002

CheckPosState <- function(curpos, curposRTS, curposSI, candle, candleRTS, candleSI,
                          cash, slippage, info, steppriceRUB) {
  cashmod <- cash
  trade1 <-  close.position(candle, curpos, slippage$MIX, instrument = "MIX")
  if (length(trade1$price)>0) {
    cashmod <- cashmod - trade1$dir * trade1$price * trade1$volume * info$point.price[info$instrument == "MIX"] -
      info$commis[info$instrument == "MIX"]*trade1$volume
  }
  trade2 <- close.position(candleRTS, curposRTS, slippage$RI, instrument = "RI")
  if (length(trade2$price)>0) {
    cashmod <- cashmod - trade2$dir * trade2$price * trade2$volume * steppriceRUB -
      info$commis[info$instrument == "RI"]*trade2$volume
  }
  trade3 <- close.position(candleSI, curposSI, slippage$SI, instrument = "SI")
  if (length(trade3$price)>0) {
    cashmod <- cashmod - trade3$dir * trade3$price * trade3$volume * info$point.price[info$instrument == "SI"] -
      info$commis[info$instrument == "SI"]*trade3$volume
  }
list(cashmod = cashmod, trade1 = trade1, trade2 = trade2, trade3 = trade3)
}

antiMMVBArbstrategyI <- function(tabMMVB, tabRTS, tabSI, tabRTSsp, ind, mask, params, curposinfo, info, cash, spreads, slippage, candletype="1s") {
  K <- KK
  maxpos <- params$maxpos
  options(stringsAsFactors=FALSE)
  trades <- create.trades()

  curpos <- curposinfo$MMVB
  curposRTS <- curposinfo$RTS
  curposSI <- curposinfo$SI

  if (params$printmode == TRUE) {
    casheff <- c()
    modeleff <- c()
    mmvbeff <- c()
    mmvbspread <- c()
    dateeff <- c()
  }

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

  candidatestoptime <- NA

  firstiter <- TRUE
  j <- 1
  modshiftprev <- 0
  modshift <- 0

  deltapos <- 0
  changeflag <- TRUE

  rr <- which(tabMMVB$Time == min(tabMMVB$Time[tabMMVB$Time >= ind$time[1]]))

  if (params$mode == 1) {
    hedgetime <- tabMMVB$Time[1]
  }
  steppriceDOL <- 0.02
  for (r in (rr+1):nrow(tabMMVB)) {
    if (mask[r] > 0) {
      candle <- tabMMVB[r, ]
      ###определение цены шага РТС в рублях
      steppriceRUB <- tabRTSsp$PriceStep[tabRTSsp$Time == max(tabRTSsp$Time[tabRTSsp$Time <= candle$Time])]
      if (length(steppriceRUB) == 0) {
        steppriceRUB <- tabRTSsp$PriceStep[1]
      }
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
      ####здесь должна быть проверка на стоп-лосс
      ####   if (curpos != 0) {
      temp <- CheckPosState(curpos, curposRTS, curposSI, candle, candleRTS, candleSI, cash, slippage, info, steppriceRUB)
      cashmod <- temp$cashmod
      trade1 <- temp$trade1
      trade2 <- temp$trade2
      trade3 <- temp$trade3
      if (firstiter) {
        cash0 <- cashmod
        firstiter <- FALSE
      }
        if (curpos != 0) {
          if ((cashmod -  cash0)/abs(curpos) < -params$stoploss) {
            if (is.na(candidatestoptime)) {
              candidatestoptime <- as.numeric(candle$Time)
            } else if (as.numeric(candle$Time) - candidatestoptime > params$dur) {
              trades <- rbind(trades, trade1)
              trades <- rbind(trades, trade2)
              trades <- rbind(trades, trade3)
              cash <- cashmod
              curpos <- 0
              curposRTS <- 0
              curposSI <- 0
              print("stoplossed")
              break
            }
          } else {
            candidatestoptime <- NA
          }
        }
      if (params$printmode) {
        casheff <- as.numeric(c(casheff, cashmod))
        modeleff <- c(modeleff, model)
        mmvbeff <- c(mmvbeff, (candle$Bid + candle$Ask)/2)
        dateeff <- c(dateeff, candle$Time)
        mmvbspread <- c(mmvbspread, (candle$Ask - candle$Bid)/2)
      }

  ####   }

      ###рехеджирование
      if (changeflag & (params$mode<3)) {
        ###выражение под if указывается в лотах, поэтому шаг цены там не нужен
        if (curposRTS > (alpha/1000)*curpos) {
          vol <- (curposRTS-(alpha/1000)*curpos)
          trade <-  sell(candleRTS, volume = vol, comment = "RTS-rehedge",
                         slippage = slippage$RI, instrument = "RTS")
          cash <- cash + trade$price * trade$volume * steppriceRUB -
            info$commis[info$instrument == "RI"]*trade$volume
          trades <- rbind(trades, trade)
          curposRTS <- (alpha/1000) * curpos
          ###а тут уже хеджируется стоимость, поэтому шаг цены  там нужен
          vol <- (curposRTS - (alpha/1000) * curpos) * candleRTS$Bid *steppriceRUB/info$point.price[info$instrument == "SI"]
          trade <- sell(candleSI, volume = vol, comment = "SI",
                        slippage = slippage$SI, instrument = "SI")
          cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
            info$commis[info$instrument == "SI"]*trade$volume
          trades <- rbind(trades, trade)
          curposSI <- curposSI - trade$volume*info$point.price[info$instrument == "SI"]
        }
        else if (curposRTS < (alpha/1000) * curpos) {
          trade <- buy(candleRTS, volume = - curposRTS + (alpha/1000) * curpos, comment = "RTS-rehedge",
                       slippage = slippage$RI, instrument = "RTS")
          cash <- cash - trade$price * trade$volume * steppriceRUB -
            info$commis[info$instrument == "RI"]*trade$volume
          trades <- rbind(trades, trade)
          curposRTS <- (alpha/1000) * curpos
          trade <- buy(candleSI, volume = (- curposRTS + (alpha/1000) * curpos) * candleRTS$Ask *
                         steppriceRUB/info$point.price[info$instrument == "SI"], comment = "SI",
                       slippage = slippage$SI, instrument = "SI")
          cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
            info$commis[info$instrument == "SI"]*trade$volume
          trades <- rbind(trades, trade)
          curposSI <- curposSI + trade$volume*info$point.price[info$instrument == "SI"]
        }
      }
      changeflag <- FALSE

      ###проверка возможности входа
      gap <- ((candle$Bid + candle$Ask)/2 - model - modshiftprev) %/% params$size

      if (gap > 0) {
        for (i in 1:(gap+1)) {
          if (params$MODE == 1) {
            if (candle$Bid >= model + modshiftprev + i*params$size) {
              if (curpos - params$multip[i] >= -maxpos) {
                modshift <- modshift + params$shift
                trade <- sell(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                              slippage = slippage$MIX,
                              limit.price = model + i*params$size + modshiftprev, instrument = "MIX")
                cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "MIX"] -
                  info$commis[info$instrument == "MIX"]*trade$volume
                trades <- rbind(trades, trade)
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
                trade <-  sell(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                               slippage = slippage$MIX, instrument = "MIX")
                cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "MIX"] -
                  info$commis[info$instrument == "MIX"]*trade$volume
                trades <- rbind(trades, trade)
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
                trade <- buy(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                             slippage = slippage$MIX,
                             limit.price = model - i*params$size + modshiftprev, instrument = "MIX")
                cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "MIX"] -
                  info$commis[info$instrument == "MIX"]*trade$volume
                trades <- rbind(trades, trade)
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
                trade <- buy(candle, volume = params$multip[i], comment = paste("MIX", modshiftprev, alpha, beta),
                             slippage = slippage$MIX, instrument = "MIX")
                cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "MIX"] -
                  info$commis[info$instrument == "MIX"]*trade$volume
                trades <- rbind(trades, trade)
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

      ###старое хеджирование
      if (params$mode < 3) {
        if (deltapos > 0) {
          #       print("RTS true spread1")
          #        print((candleRTS$Ask - candleRTS$Bid)/2)
          #        print(candleRTS)
          trade <- sell(candleRTS, volume = alpha*abs(deltapos)/1000, comment = "RTS", slippage = slippage$RI,
                      instrument = "RI")
          cash <- cash + trade$price * trade$volume * steppriceRUB -
            info$commis[info$instrument == "RI"]*trade$volume
          trades <- rbind(trades, trade)
          curposRTS <- curposRTS - abs(deltapos) * alpha/1000
          #       print("SI true spread")
          #        print((candleSI$Ask - candleSI$Bid)/2)

          trade <- sell(candleSI, volume = alpha*abs(deltapos)*candleRTS$Bid*0.001*
                          steppriceRUB/(info$point.price[info$instrument == "SI"] * candleSI$Bid), comment = "SI",
                        slippage = slippage$SI, instrument = "SI")
          cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
            info$commis[info$instrument == "SI"]*trade$volume
          trades <- rbind(trades, trade)
          curposSI <- curposSI - trade$volume
          deltapos <- 0
        } else if (deltapos < 0) {
          #       print("RTS true spread2")
          #        print((candleRTS$Ask - candleRTS$Bid)/2)
          trade <- buy(candleRTS, volume = alpha*abs(deltapos)/1000, comment = "RTS", slippage = slippage$RI,
                       instrument = "RI")
          cash <- cash - trade$price * trade$volume * steppriceRUB -
            info$commis[info$instrument == "RI"]*trade$volume
          trades <- rbind(trades, trade)
          curposRTS <- curposRTS + abs(deltapos) * alpha/1000
          #       print("SI true spread")
          #        print((candleSI$Ask - candleSI$Bid)/2)

          trade <- buy(candleSI,
                       volume = alpha*abs(deltapos)*candleRTS$Ask*0.001*steppriceRUB/
                         (info$point.price[info$instrument == "SI"]*candleSI$Ask),
                       comment = "SI",
                       slippage = slippage$SI, instrument = "SI")
          cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
            info$commis[info$instrument == "SI"]*trade$volume
          trades <- rbind(trades, trade)
          curposSI <- curposSI + trade$volume
          deltapos <- 0
        }
      } else if (params$mode == 3) {
        ###truepos --- стоимость позы
        trueposMIX <- TruePos(curpos, candle, info$point.price[info$instrument == "MIX"])
        trueposRTS <- TruePos(curposRTS, candleRTS, steppriceRUB )
        deltaexpr <- trueposMIX + trueposRTS
        if (abs(deltaexpr) > params$absval) {
          #print("delt")
          #print(deltaexpr)
          if (deltaexpr > 0) {
             vol <- deltaexpr/(steppriceRUB*candleRTS$Bid)
             trade <- sell(candleRTS, volume = vol, comment = "RTS-hedge", instrument = "RI")
             cash <- cash + trade$price * trade$volume * steppriceRUB -
               info$commis[info$instrument == "RI"]*trade$volume
             trades <- rbind(trades, trade)
       #      print(c(curposRTS, curposRTS-vol,-vol))
             curposRTS <- curposRTS - vol

             #trueposMIX <- TruePos(curpos, candle, info$point.price[info$instrument == "MIX"] )
        #    print(curposRTS)
        #    print(candleRTS)
             #trueposRTS <- TruePos(curposRTS, candleRTS, steppriceRUB )
             #print(c(trueposMIX + trueposRTS))
      #       stop("")

       #     print(c(curposRTS, curpos))
      #       print(vol)
      #       print(candleRTS$Bid)
      #       print(info$point.price[info$instrument == "RI"]/info$point.price[info$instrument == "SI"])
             vol <- vol * candleRTS$Bid *
               steppriceRUB/(info$point.price[info$instrument == "SI"]*candleSI$Bid)
        #     print("h1")
        #     print(vol)
             trade <- sell(candleSI, volume = vol, comment = "SI-hedge", instrument = "SI")
             cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
               info$commis[info$instrument == "SI"]*trade$volume
             curposSI <- curposSI - vol
             trades <- rbind(trades, trade)
          } else if (deltaexpr < 0) {
            vol <- -deltaexpr/(steppriceRUB*candleRTS$Ask)
            trade <- buy(candleRTS, volume = vol, comment = "RTS-hedge", instrument = "RI")
            cash <- cash - trade$price * trade$volume * steppriceRUB -
              info$commis[info$instrument == "SI"]*trade$volume
            trades <- rbind(trades, trade)
            curposRTS <- curposRTS + vol
            vol <- curposRTS * candleRTS$Ask *
              steppriceRUB/(info$point.price[info$instrument == "SI"]*candleSI$Ask)
     #       print("h2")
    #        print(vol)
            trade <- buy(candleSI, volume = vol, comment = "SI-hedge", instrument = "SI")
            cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
              info$commis[info$instrument == "SI"]*trade$volume
            curposSI <- curposSI + vol
            trades <- rbind(trades, trade)
          }
        }
      }
      if (params$mode == 1) {
        ###рехеджирование по времени
        if (as.numeric(candle$Time - hedgetime, units="secs") >= params$deltatime) {
          redeltapos <- GetDeltaPos(curposRTS, curposSI, candleRTS, candleSI, steppriceDOL)$deltapos
          if (redeltapos < -0.1) {
            trade <- sell(candleSI, volume = abs(redeltapos)/info$point.price[info$instrument == "SI"], comment = "SI-rehedge",
                          slippage = slippage$SI, instrument = "SI")
            cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
              info$commis[info$instrument == "SI"]*trade$volume
            trades <- rbind(trades, trade)
            curposSI <- curposSI - abs(redeltapos)
          } else if (redeltapos > 0.1) {
            trade <-  buy(candleSI, volume = abs(redeltapos)/info$point.price[info$instrument == "SI"], comment = "SI-rehedge",
                          slippage = slippage$SI, instrument = "SI")
            cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
              info$commis[info$instrument == "SI"]*trade$volume
            trades <- rbind(trades, trade)
            curposSI <- curposSI + abs(redeltapos)
          }
          hedgetime <- candle$Time
        }
      } else if (params$mode >= 2) {
        ###рехеджирование относительное
        temp <- GetDeltaPos(curposRTS, curposSI, candleRTS, candleSI, steppriceDOL)
        redeltapos <- temp$deltapos
        truepos <- temp$pos
       #### if (abs(redeltapos/truepos) != 0) {print(abs(redeltapos/truepos)); print(c(curposRTS, curposSI)); print(c(redeltapos, truepos)) }
        if (abs(redeltapos/truepos) > params$critval) {

          if (redeltapos < 0) {
            vol <- abs(redeltapos)/info$point.price[info$instrument == "SI"]
            trade <- sell(candleSI, volume = vol, comment = "SI-rehedge",
                          slippage = slippage$SI, instrument = "SI")
            cash <- cash + trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
              info$commis[info$instrument == "SI"]*trade$volume
            trades <- rbind(trades, trade)
            curposSI <- curposSI - vol
          } else if (redeltapos > 0) {
            vol <- abs(redeltapos)/info$point.price[info$instrument == "SI"]
            trade <- buy(candleSI, volume = vol, comment = "SI-rehedge",
                         slippage = slippage$SI, instrument = "SI")
            cash <- cash - trade$price * trade$volume * info$point.price[info$instrument == "SI"] -
              info$commis[info$instrument == "SI"]*trade$volume
            trades <- rbind(trades, trade)
            curposSI <- curposSI + vol
          }
        }

      # if (abs(modeleff[length(modeleff)] - mmvbeff[length(mmvbeff)]) >= params$size) {
      #   print("here")
      #   print(candle)
      #   print(modeleff[length(modeleff)])
      #   print(mmvbeff[length(mmvbeff)])
      #   print((candle$Bid + candle$Ask)/2)
      # }

      }
      if (mask[r] == 2) {
        trade <- create.trades()
        trade <- close.position(candle, curpos, slippage$MIX, instrument = "MIX")
        if (length(trade$price)>0) {
          cash <- cash - trade$dir*trade$price*trade$volume*info$point.price[info$instrument == "MIX"] -
            info$commis[info$instrument == "MIX"]*trade$volume
        }

        trades <- rbind(trades, trade)
        curpos <- 0
        trade <- close.position(candleRTS, curposRTS, slippage$RI, instrument = "RI")
        if (length(trade$price)>0) {
          cash <- cash - trade$dir*trade$price*trade$volume*steppriceRUB -
            info$commis[info$instrument == "RI"]*trade$volume
        }
        trades <- rbind(trades, trade)

        curposRTS <- 0
        trade <- close.position(candleSI, curposSI, slippage$SI, instrument = "SI")
        if (length(trade$price)>0) {
          cash <- cash - trade$dir*trade$price*trade$volume*info$point.price[info$instrument == "SI"] -
            info$commis[info$instrument == "SI"]*trade$volume
        }
        trades <- rbind(trades, trade)
        curposSI <- 0
      }

    }
  }

  temp <- CheckPosState(curpos, curposRTS, curposSI, candle, candleRTS, candleSI, cash, slippage, info, steppriceRUB)
  cash <- temp$cashmod

  trades$time <- as.POSIXct(trades$time, origin = "1970-01-01", tz = "UTC")
  curposinfo$MMVB <- curpos
  curposinfo$RTS <- curposRTS
  curposinfo$SI <- curposSI
  list(trades = trades, curposinfo = curposinfo, cash = cash, cashday = cash - cash0,
       printinfo = data.frame(date = as.POSIXct(dateeff, origin = "1970-01-01", tz = "UTC"),
                              model = modeleff, cash = casheff, mmvb = mmvbeff, mmvbspread = mmvbspread))
}
