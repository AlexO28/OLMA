#requires Research.R
expinfo <- data.frame(start = c(as.Date("2016-01-15"), as.Date("2016-03-16")),
                      end = c(as.Date("2016-03-15"), Sys.Date()),
                      shortname = c("H6", "M6"), fullname = c(NA, NA))

PrepareDataForTraders <- function(datestart, dateend, expinfo, nextdate, critdate) {
  tab <- FillStavkaTab(datestart, dateend, candletype = "1mVol50", expinfo = expinfo, type = "fut")
  tab <- PrepareModel(tab, critdate, nextdate)
  tab <- FillAvStavkas(tab)
}
AnalyzeThisMean <- function(tab, num, datestart, dateend) {
  tab <- FillMean(tab, "avstavka1", num, "exponential")
  tab <- FillMean(tab, "avstavka2", num, "exponential")
  tab <- MakeReal(tab, "avstavka1exp", FALSE)
  tab <- MakeReal(tab, "avstavka2exp", TRUE)
  tab[, lintrend := NULL]
#  tab <- na.omit(tab)
  tab[Date >= datestart & Date <= dateend, ]
}
DoublePlotForTraders <- function(tab, filename, col1, col2, mainstr, level2 = 0.95, level1 = 0.6, multip = 1, onegraph = TRUE) {
  print(paste0("Writing data to file", filename))
  print(unique(tab$Date))
  print(c(col1, col2))
  vec1 <- tab[, col1, with = FALSE][[1]]*multip
  vec2 <- tab[, col2, with = FALSE][[1]]*multip
  quant2 <- quantile(abs(vec2 - vec1), level2, na.rm = TRUE)
  print(paste0("quantiles of level ", level2, " is +/- ", quant2))
  quant1 <- quantile(abs(vec2 - vec1), level1, na.rm = TRUE)
  print(paste0("quantiles of level ", level1, " is +/- ", quant1))
  jpeg(paste0(filename, ".jpeg"), width = 1900, height = 1200)
  plot(1:nrow(tab), vec1, main = mainstr)
  points(1:nrow(tab), vec2, col = "red")
  points(1:nrow(tab), vec2 + quant2, col = "blue")
  points(1:nrow(tab), vec2 - quant2, col = "blue")
  points(1:nrow(tab), vec2 + quant1, col = "green")
  points(1:nrow(tab), vec2 - quant1, col = "green")
  dev.off()
}
VisualizeThisMean <- function(tab, num, mainstr, filename, datestart, dateend) {
  tab <- data.table(as.data.frame(tab))
  tab <- AnalyzeThisMean(tab, num, datestart, dateend)
  DoublePlotForTraders(tab, paste0(filename, "Average"), "avstavka2", "avstavka2exp",
                       paste0(mainstr, "Приведено к процентам годовых."), multip = 100*365)
  DoublePlotForTraders(tab, paste0(filename, "Real"), "stavka", "avstavka2expreal",
                       paste0(mainstr))
  DoublePlotForTraders(tab, paste0(filename, "AverageMod"), "avstavka1", "avstavka1exp",
                       paste0(mainstr))
  DoublePlotForTraders(tab, paste0(filename, "RealMod"), "stavka", "avstavka1expreal",
                       paste0(mainstr))
  #tab
}
MainTraderResearcher <- function(tab) {
  datestart <- as.Date("2016-04-01")
  dateend <- as.Date("2016-05-07")
  MiniCycle <- function(mainstr, filestr) {
    num <- 7*768
    VisualizeThisMean(tab, num, paste0(mainstr, " Скользящее среднее за 7 дней"), paste0(filestr, "7days"), datestart, dateend)
    num <- 3*768
    VisualizeThisMean(tab, num, paste0(mainstr, " Скользящее среднее за 3 дня."), paste0(filestr, "3days"), datestart, dateend)
    num <- 1*768
    VisualizeThisMean(tab, num, paste0(mainstr, " Скольщящее среднее за 1 день."), paste0(filestr, "1day"), datestart, dateend)
  }
  MiniCycle("Данные с начала апреля.", "PlotFromApril")
  datestart <- as.Date("2016-03-16")
  MiniCycle("Данные с середины марта.", "PlotFromMidMarch")
  datestart <- as.Date("2016-02-15")
  MiniCycle("Данные с середины февраля.", "PlotFromMidFeb")
  datestart <- as.Date("2016-01-18")
  MiniCycle("Данные с середины января.", "PlotFromMidJan")
}

