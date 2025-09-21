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
CheckAbstractCrit <- function(tab, crit, step = 60,
                              signum = 1, fname = "CheckAbstractCritServe") {
  crit <- crit[crit$crit == signum, ]
  sapply(crit[, 2], FUN = fname, tab = tab, step = step, signum = signum)
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
CheckAbstractCritServeUp <- function(crit, tab, step, signum) {
  if (signum > 0 ) {
    curval <- tab$modelminus[crit]
  } else {
    curval <- tab$modelplus[crit]
  }

  tp <- 200

  if (signum > 0) {
  #  nextval <- min(tab$modelplus[crit:min(crit + step, nrow(tab))])
    nextval <- as.numeric(quantile(tab$modelplus[crit:min(crit + step, nrow(tab))], 0.1))
  #  nextval <- ifelse(curval - nextval >= tp, tp, tab$modelplus[min(crit + step, nrow(tab))])
  #  nextval <- as.numeric(quantile(tab$modelplus[crit:min(crit + step, nrow(tab))], 0.25))
    return(curval - nextval)
  } else {
    #nextval <- max(tab$modelminus[crit:min(crit + step, nrow(tab))])
    nextval <- as.numeric(quantile(tab$modelplus[crit:min(crit + step, nrow(tab))], 0.9))
    #nextval <- ifelse(nextval - curval >= tp, tp, tab$modelminus[min(crit + step, nrow(tab))])
    #nextval <- as.numeric(quantile(tab$modelplus[crit:min(crit + step, nrow(tab))], 0.75))
    return(nextval - curval)
  }
}
CalculateAnnualizedStavka <- function(tab) {
  tab$stavkamix <- 100*365*(tab$mix.close/100 - tab$mixind.close)/(tab$mixind.close*tab$daystillexpir)
  tab$stavkarts <- 100*365*(tab$rts.close/100 - tab$rtsind.close)/(tab$rtsind.close*tab$daystillexpir)
  tab$stavkasi <- 100*365*(tab$si.close/1000 - tab$siind.close)/(tab$siind.close*tab$daystillexpir)
  tab
}
