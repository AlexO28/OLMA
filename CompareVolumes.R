GetStavka <- function(tabspot, tabfut, dirspot, dirfut) {
  tabspot <- tabspot[tabspot$Side == dirspot, ]
  tabfut <- tabfut[tabfut$Side == dirfut, ]
  if (dirfut == "Buy") {
    stavka <- -(tabfut$Price - tabspot$Price*1000)
  } else {
    stavka <- (tabfut$Price - tabspot$Price*1000) 
  }
stavka
}