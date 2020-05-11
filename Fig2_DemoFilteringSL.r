
# Plotting code for Fig 2. Interplay between demographic filtering and social learning

# Load function "recurs" from file "Recursions.R"


sim_IL_Fert <- recurs(s = c(0.85, 0.85), b = c(0.35, 0.5), IL_Only = "Yes", Stoch_E = "Yes")
sim_IL_Viab <- recurs(s = c(0.85, 0.93), b = c(0.35, 0.35), IL_Only = "Yes", Stoch_E = "Yes")

sim_SL_Fert <- recurs(s = c(0.85, 0.85), b = c(0.35, 0.5), IL_Only = "No", Stoch_E = "Yes")
sim_SL_Viab <- recurs(s = c(0.85, 0.93), b = c(0.35, 0.35), IL_Only = "No", Stoch_E = "Yes")


#calculate time since change

sim_IL_Fert$TimeSinceChange <- c()
for (i in 1:10000) {
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
for (i in 1:10000) {
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
for (i in 1:10000) {
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
for (i in 1:10000) {
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
plot(apply(sim_IL_Viab$AdaptedPerAge, 1, mean), type = "l",lty=2, lwd=2, ylim = c(0,1), ylab = "", xlab = "")
mtext("Proportion Adapted per Age", side = 2, line = 3)
mtext("Age", side = 1, line = 2.3)
mtext("Individual Learners", side = 3, line = 0.5, cex = 1.2)
legend("topleft", "A", bty="n")


plot(apply(sim_SL_Fert$AdaptedPerAge, 1, mean), type = "l", lwd=2, ylim = c(0,1), ylab = "", xlab = "")
par(new=TRUE)
plot(apply(sim_SL_Viab$AdaptedPerAge, 1, mean), type = "l",lty=2, lwd=2, ylim = c(0,1), ylab = "", xlab = "")
mtext("Age", side = 1, line = 2.3)
mtext("Individual + Social Learners", side = 3, line = 0.5, cex = 1.2)
legend("topright", title = "Selection on", c("Fecundity (No Filtering)", "Viability (Demogr. Filtering)"), lty = c(1,2), lwd = 2, bty = "n")
legend("topleft", "B", bty="n")


plot(sim_IL_Fert$AdaptPerTime, type = "l", lwd=2, ylim = c(0,1), xlim = c(0,150), ylab = "", xlab = "")
par(new=TRUE)
plot(sim_IL_Viab$AdaptPerTime, type = "l", lwd=2, lty=2, ylim = c(0,1), xlim = c(0,150), ylab = "", xlab = "")
abline(h=0.5, lty= 3)
legend("topleft", "C", bty="n")

mtext("Proportion Adapted after Change", side = 2, line = 3)
mtext("Time Since Change", side = 1, line = 2.3)


plot(sim_SL_Fert$AdaptPerTime, type = "l", lwd=2, ylim = c(0,1), xlim = c(0,150), ylab = "", xlab = "")
par(new=TRUE)
plot(sim_SL_Viab$AdaptPerTime, type = "l", lwd=2, lty=2, ylim = c(0,1), xlim = c(0,150), ylab = "", xlab = "")
abline(h=0.5, lty= 3)
mtext("Time Since Change", side = 1, line = 2.3)
legend("topleft", "D", bty="n")

