layout(matrix(1:4, nrow = 2))
plot(mmvb1[, 3], mmvb1[, 5])
abline(v = 33*60)
plot(mmvb2[, 3], mmvb2[, 5])
abline(v = 33*60)
plot(mmvb3[, 3], mmvb3[, 5])
abline(v = 33*60)
plot(mmvb4[, 3], mmvb4[, 5])
abline(v = 33*60)


plot(mmvblog[seq(1, len, 4), 3], mmvblog[seq(1, len, 4), 5])
plot(mmvblog[seq(2, len, 4), 3], mmvblog[seq(2, len, 4), 5])
plot(mmvblog[seq(3, len, 4), 3], mmvblog[seq(3, len, 4), 5])
plot(mmvblog[seq(4, len, 4), 3], mmvblog[seq(4, len, 4), 5])

layout(matrix(1:4, nrow = 2))
plot(tab1min$mix.close - (tab1min$modelask + tab1min$modelbid)/2, type = "l", main = "1 minutes", ylim = c(-125, 125))
plot(tab2min$mix.close - (tab2min$modelask + tab2min$modelbid)/2, type = "l", main = "2 minutes", ylim = c(-125, 125))
plot(tab3min$mix.close - (tab3min$modelask + tab3min$modelbid)/2, type = "l", main = "3 minutes", ylim = c(-125, 125))
plot(tab5min$mix.close - (tab5min$modelask + tab5min$modelbid)/2, type = "l", main = "5 minutes", ylim = c(-125, 125))

mmvblog <- read.table("mmvblognew.txt", header = FALSE, sep = ",")
mmvb1 <- mmvblog[mmvblog[, 1] == 100 & mmvblog[, 4] == 0, ]
mmvb2 <- mmvblog[mmvblog[, 1] == 100 & mmvblog[, 4] == 1, ]
mmvb3 <- mmvblog[mmvblog[, 1] == 125 & mmvblog[, 4] == 0, ]
mmvb4 <- mmvblog[mmvblog[, 1] == 125 & mmvblog[, 4] == 1, ]
mmvb1 <- mmvb1[order(mmvb1[, 3]), ]
mmvb2 <- mmvb2[order(mmvb2[, 3]), ]
mmvb3 <- mmvb3[order(mmvb3[, 3]), ]
mmvb4 <- mmvb4[order(mmvb4[, 3]), ]
layout(matrix(1:4, nrow = 2))
plot(mmvb1[, 3], mmvb1[, 5])
plot(mmvb2[, 3], mmvb2[, 5])
plot(mmvb3[, 3], mmvb3[, 5])
plot(mmvb4[, 3], mmvb4[, 5])
