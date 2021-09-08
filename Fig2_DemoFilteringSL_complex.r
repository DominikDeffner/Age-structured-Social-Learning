





graphics.off()

png("FigS222AdaptPerAgeTime.png", width = 25,height = 12, units = "cm", res = 900)



colfunc<-  colorRampPalette(colors=c("Darkred", "red", "orange", "Yellow"))(8)


par(mfrow = c(2,4), 
    mar= c(2,2.5,1,1.5), 
    oma =c(2.1,5,1.4,0))


## Fecundity 

A <- matrix(0, nrow = 80, ncol = 100)

for (i in 1:100) {
  A[,i] <- apply(sim_IL_Fert$AdaptedPerAge[, which(sim_IL_Fert$TimeSinceChange == i)], 1, mean)
}

plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
  
  lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 0.6), lwd = 2)
}
lines(apply(sim_IL_Fert$AdaptedPerAge, 1, mean), lwd = 2)
mtext("IL", side = 3, line = 0.5, cex = 1.1)
mtext("Fecundity selection", side = 2, line = 6, cex = 1.1)
legend("topleft", "A", bty="n")



par(mar= c(2,1,1,2.5))


A <- matrix(0, nrow = 80, ncol = 100)

for (i in 1:100) {
  A[,i] <- apply(sim_SL_Fert$AdaptedPerAge[, which(sim_SL_Fert$TimeSinceChange == i)], 1, mean)
}

plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
  
  lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 0.6), lwd = 2)
}
lines(apply(sim_SL_Fert$AdaptedPerAge, 1, mean), lwd = 2)
legend("topleft", "B", bty="n")

mtext("IL and SL", side = 3, line = 0.5, cex = 1.1)
legend("topright", title = "Time since change", legend = c(1,10, 20, 30, 40, 50, 75, 100), col = colfunc, lty = 1,lwd=2, ncol = 2, cex = 1, bty = "n")


par(mar=c(2,2.5,1,1.5))






colfunc<-  colorRampPalette(colors=c("Darkblue","Blue", "Green", "Lightgreen"))(8)

A <- matrix(0, nrow = 80, ncol = 200)

for (i in 1:200) {
  A[,i] <- apply(sim_IL_Fert$AdaptedPerAge[, which(sim_IL_Fert$TimeSinceChange == i)], 1, mean)
}

plot(A[1,], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 60, 70)) {
  
  lines(A[i,], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,60, 70)==i)], alpha = 0.6), lwd = 2)
}
lines(sim_IL_Fert$AdaptPerTime, lwd = 2)
mtext("IL", side = 3, line = 0.5, cex = 1.1)
legend("topleft", "C", bty="n")



par(mar=c(2,1,1,2))


A <- matrix(0, nrow = 80, ncol = 200)

for (i in 1:200) {
  A[,i] <- apply(sim_SL_Fert$AdaptedPerAge[, which(sim_SL_Fert$TimeSinceChange == i)], 1, mean)
}

plot(A[1,], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 60, 70)) {
  
  lines(A[i,], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,60, 70)==i)], alpha = 0.6), lwd = 2)
}
lines(sim_SL_Fert$AdaptPerTime, lwd = 2)
legend("topleft", "D", bty="n")
legend("topright", title = "Age class", legend = c(1,10, 20, 30, 40, 50, 60, 70), col = colfunc, lty = 1,lwd=2, ncol = 2, cex = 1, bty = "n")

mtext("IL and SL", side = 3, line = 0.5, cex = 1.1)







par(mar=c(2,2.5,1,1.5))


#
#
# Viability
#
#
#
colfunc<-  colorRampPalette(colors=c("Darkred", "red", "orange", "Yellow"))(8)


A <- matrix(0, nrow = 80, ncol = 100)

for (i in 1:100) {
  A[,i] <- apply(sim_IL_Viab$AdaptedPerAge[, which(sim_IL_Viab$TimeSinceChange == i)], 1, mean)
}

plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
  
  lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 0.6), lwd = 2)
}
lines(apply(sim_IL_Viab$AdaptedPerAge, 1, mean), lwd = 2)
mtext("Viability selection", side = 2, line = 6, cex = 1.1)

legend("topleft", "E", bty="n")


par(mar= c(2,1,1,2.5))


A <- matrix(0, nrow = 80, ncol = 100)
for (i in 1:100) {
  A[,i] <- apply(sim_SL_Viab$AdaptedPerAge[, which(sim_SL_Viab$TimeSinceChange == i)], 1, mean)
}

plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
  
  lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 0.6), lwd = 2)
}

lines(apply(sim_SL_Viab$AdaptedPerAge, 1, mean), lwd = 2)
legend("topleft", "F", bty="n")



par(mar=c(2,2.5,1,1.5))



colfunc<-  colorRampPalette(colors=c("Darkblue","Blue", "Green", "Lightgreen"))(8)



A <- matrix(0, nrow = 80, ncol = 200)
for (i in 1:200) {
  A[,i] <- apply(sim_IL_Viab$AdaptedPerAge[, which(sim_IL_Viab$TimeSinceChange == i)], 1, mean)
}
plot(A[1,], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 60, 70)) {
  
  lines(A[i,], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,60, 70)==i)], alpha = 0.6), lwd = 2)
}


lines(sim_IL_Viab$AdaptPerTime, lwd = 2)

legend("topleft", "G", bty="n")


par(mar=c(2,1,1,2))

A <- matrix(0, nrow = 80, ncol = 200)
for (i in 1:200) {
  A[,i] <- apply(sim_SL_Viab$AdaptedPerAge[, which(sim_SL_Viab$TimeSinceChange == i)], 1, mean)
}

plot(A[1,], type = "n", ylim = c(0,1), ylab = "", xlab = "")

for (i in c(1,10, 20, 30, 40, 50, 60, 70)) {
  
  lines(A[i,], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,60, 70)==i)], alpha = 0.6), lwd = 2)
}

lines(sim_SL_Viab$AdaptPerTime, lwd = 2)
legend("topleft", "H", bty="n")




mtext(side = 2, line = 0.7 , "Proportion Adapted", cex = 1, outer = TRUE)
mtext(side = 1, line = 1 , "        Age                                                                        Time since change", cex = 1.1, outer = TRUE)


dev.off()
