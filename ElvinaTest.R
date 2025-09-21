StudySpotDeals <- function(tab, secname) {
  tabred <- tab[tab$Security == secname, ]
  table(tabred$Volume)
}