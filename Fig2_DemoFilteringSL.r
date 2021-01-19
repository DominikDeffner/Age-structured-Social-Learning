
# Plotting code for Fig 2. Interplay between demographic filtering and social learning

# Load function "recurs" from file "Recursions.R"


sim_IL_Fert <- recurs(tmax=100000, s = c(0.85, 0.85), b = c(0.35, 0.5), IL_Only = "Yes", Stoch_E = "Yes")
sim_IL_Viab <- recurs(tmax=100000, s = c(0.85, 0.93), b = c(0.35, 0.35), IL_Only = "Yes", Stoch_E = "Yes")

sim_SL_Fert <- recurs(tmax=100000, s = c(0.85, 0.85), b = c(0.35, 0.5), IL_Only = "No", Stoch_E = "Yes")
sim_SL_Viab <- recurs(tmax=100000, s = c(0.85, 0.93), b = c(0.35, 0.35), IL_Only = "No", Stoch_E = "Yes")


#calculate time since change

sim_IL_Fert$TimeSinceChange <- c()
for (i in 1:100000) {
  print(i)
  if (i <= min(which(sim_IL_Fert$Change==1))){
    sim_IL_Fert$TimeSinceChange[i] <- i
  } else {
    sim_IL_Fert$TimeSinceChange[i] <- i - max(which(sim_IL_Fert$Change==1)[which(sim_IL_Fert$Change==1)<i])
    }
}

sim_IL_Fert$AdaptPerTime <- c()
for (i in unique(sim_IL_Fert$TimeSinceChange)) {
  sim_IL_Fert$AdaptPerTime[i] <- mean(sim_IL_Fert$Adapt[which(sim_IL_Fert$TimeSinceChange == i)])
}


sim_IL_Viab$TimeSinceChange <- c()
for (i in 1:100000) {
  if (i <= min(which(sim_IL_Viab$Change==1))){
    sim_IL_Viab$TimeSinceChange[i] <- i
  } else {
    sim_IL_Viab$TimeSinceChange[i] <- i - max(which(sim_IL_Viab$Change==1)[which(sim_IL_Viab$Change==1)<i])
  }
}

sim_IL_Viab$AdaptPerTime <- c()
for (i in unique(sim_IL_Viab$TimeSinceChange)) {
  sim_IL_Viab$AdaptPerTime[i] <- mean(sim_IL_Viab$Adapt[which(sim_IL_Viab$TimeSinceChange == i)])
}






#SL
sim_SL_Fert$TimeSinceChange <- c()
for (i in 1:100000) {
  if (i <= min(which(sim_SL_Fert$Change==1))){
    sim_SL_Fert$TimeSinceChange[i] <- i
  } else {
    sim_SL_Fert$TimeSinceChange[i] <- i - max(which(sim_SL_Fert$Change==1)[which(sim_SL_Fert$Change==1)<i])
  }
}

sim_SL_Fert$AdaptPerTime <- c()
for (i in unique(sim_SL_Fert$TimeSinceChange)) {
  sim_SL_Fert$AdaptPerTime[i] <- mean(sim_SL_Fert$Adapt[which(sim_SL_Fert$TimeSinceChange == i)])
}


sim_SL_Viab$TimeSinceChange <- c()
for (i in 1:100000) {
  if (i <= min(which(sim_SL_Viab$Change==1))){
    sim_SL_Viab$TimeSinceChange[i] <- i
  } else {
    sim_SL_Viab$TimeSinceChange[i] <- i - max(which(sim_SL_Viab$Change==1)[which(sim_SL_Viab$Change==1)<i])
  }
}

sim_SL_Viab$AdaptPerTime <- c()
for (i in unique(sim_SL_Viab$TimeSinceChange)) {
  sim_SL_Viab$AdaptPerTime[i] <- mean(sim_SL_Viab$Adapt[which(sim_SL_Viab$TimeSinceChange == i)])
}









par(mfrow = c(2,2),
    mar = c(3.8,2.5,0,0.1),
    oma = c(0,2,2,0.1))


plot(apply(sim_IL_Fert$AdaptedPerAge, 1, mean) , type = "l", lwd=2, ylim = c(0,1), ylab = "", xlab = "")
par(new=TRUE)
plot(apply(sim_IL_Viab$AdaptedPerAge, 1, mean),col= "darkred", type = "l",lty=2, lwd=2, ylim = c(0,1), ylab = "", xlab = "")
mtext("Proportion Adapted per Age", side = 2, line = 3)
mtext("Age", side = 1, line = 2.3)
mtext("Individual Learners", side = 3, line = 0.5, cex = 1.2)
legend("topleft", "A", bty="n")
abline(h=0.5, lty= 3)


plot(apply(sim_SL_Fert$AdaptedPerAge, 1, mean), type = "l", lwd=2, ylim = c(0,1), ylab = "", xlab = "")
par(new=TRUE)
plot(apply(sim_SL_Viab$AdaptedPerAge, 1, mean), col= "darkred",type = "l",lty=2, lwd=2, ylim = c(0,1), ylab = "", xlab = "")
mtext("Age", side = 1, line = 2.3)
mtext("Individual + Social Learners", side = 3, line = 0.5, cex = 1.2)
legend("topright", title = "Selection on", c("Fecundity (No Filtering)", "Viability (Demogr. Filtering)"), lty = c(1,2), col= c("black","darkred"), lwd = 2, bty = "n")
legend("topleft", "B", bty="n")
abline(h=0.5, lty= 3)


plot(sim_IL_Fert$AdaptPerTime, type = "l", lwd=2, ylim = c(0,1), xlim = c(0,200), ylab = "", xlab = "")
par(new=TRUE)
plot(sim_IL_Viab$AdaptPerTime, type = "l", lwd=2, col= "darkred",lty=2, ylim = c(0,1), xlim = c(0,200), ylab = "", xlab = "")
abline(h=0.5, lty= 3)
legend("topleft", "C", bty="n")

mtext("Proportion Adapted after Change", side = 2, line = 3)
mtext("Time Since Change", side = 1, line = 2.3)


plot(sim_SL_Fert$AdaptPerTime, type = "l", lwd=2, ylim = c(0,1), xlim = c(0,200), ylab = "", xlab = "")
par(new=TRUE)
plot(sim_SL_Viab$AdaptPerTime, type = "l", col= "darkred", lwd=2, lty=2, ylim = c(0,1), xlim = c(0,200), ylab = "", xlab = "")
abline(h=0.5, lty= 3)
mtext("Time Since Change", side = 1, line = 2.3)
legend("topleft", "D", bty="n")














png("FigS2AdaptPerAgeTime.png", width = 18,height = 16, units = "cm", res = 900)

colfunc<-  colorRampPalette(colors=c("Darkred","Yellow"))(8)
 

par(mfrow = c(2,2),
     mar = c(3,2.5,0,0.1),
     oma = c(1.5,3.5,2,0.1))
 
 A <- matrix(0, nrow = 80, ncol = 100)
 
 for (i in 1:100) {
   A[,i] <- apply(sim_IL_Fert$AdaptedPerAge[, which(sim_IL_Fert$TimeSinceChange == i)], 1, mean)
 }
 
 plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")
 
 for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
   
   lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 1), lwd = 1.5)
 }
 lines(apply(sim_IL_Fert$AdaptedPerAge, 1, mean), lwd = 2)
 mtext("Individual Learners", side = 3, line = 0.5, cex = 1.1)
 
 mtext("Fecundity selection", side = 2, line = 4.5, cex = 1.1)
 legend("topleft", "A", bty="n")
 
 
 legend("topright", title = "Time since change in the environment", legend = c(1,10, 20, 30, 40, 50, 75, 100), col = colfunc, lty = 1,lwd=2, ncol = 2, cex = 1, bty = "n")
 
 
 
 
 A <- matrix(0, nrow = 80, ncol = 100)
 
 for (i in 1:100) {
   A[,i] <- apply(sim_SL_Fert$AdaptedPerAge[, which(sim_SL_Fert$TimeSinceChange == i)], 1, mean)
 }
 
 plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")
 
 for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
   
   lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 1), lwd = 1.5)
 }
 lines(apply(sim_SL_Fert$AdaptedPerAge, 1, mean), lwd = 2)
 legend("topleft", "B", bty="n")
 
 mtext("Individual + Social Learners", side = 3, line = 0.5, cex = 1.1)
 
 
 A <- matrix(0, nrow = 80, ncol = 100)
 
for (i in 1:100) {
   A[,i] <- apply(sim_IL_Viab$AdaptedPerAge[, which(sim_IL_Viab$TimeSinceChange == i)], 1, mean)
 }
 
 plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")
 
 for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
       
       lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 1), lwd = 1.5)
     }
 lines(apply(sim_IL_Viab$AdaptedPerAge, 1, mean), lwd = 2)
 mtext("Viability selection", side = 2, line = 4.5, cex = 1.1)
 
 legend("topleft", "C", bty="n")
 
 
 
 A <- matrix(0, nrow = 80, ncol = 100)
 for (i in 1:100) {
   A[,i] <- apply(sim_SL_Viab$AdaptedPerAge[, which(sim_SL_Viab$TimeSinceChange == i)], 1, mean)
 }
 
 plot(A[,1], type = "n", ylim = c(0,1), ylab = "", xlab = "")
 
  for (i in c(1,10, 20, 30, 40, 50, 75, 100)) {
    
    lines(A[,i], col=alpha(colfunc[which(c(1,10, 20, 30, 40, 50,75, 100)==i)], alpha = 1), lwd = 1.5)
  }
 
 lines(apply(sim_SL_Viab$AdaptedPerAge, 1, mean), lwd = 2)
 legend("topleft", "D", bty="n")
 
 

mtext(side = 2, line = 0 , "Proportion Adapted", cex = 1.1, outer = TRUE)
 mtext(side = 1, line = 0 , "Age", cex = 1.1, outer = TRUE)
 
  dev.off()

