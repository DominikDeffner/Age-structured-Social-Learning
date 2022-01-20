
# Plotting code for Fig.6 and Fig.S3 (heatmaps). Proportions of social learners and old bias
# for the temporal individual-based model including adult learning

# Plots results from "ABM_Simulations.r"


# Remove first 2000 timesteps and calculate mean for each parameter combination


MeanILAdult <- matrix(NA, nrow = nrow(seq_adult), ncol = 10 )
MeanOldAdult <- matrix(NA, nrow = nrow(seq_adult), ncol = 10 )

for (i in 1:nrow(seq_adult)){
  for (j in 1:10) {
    result_adult[[i]][[j]]$PropIL <- result_adult[[i]][[j]]$PropIL[-(1:2000)]
    result_adult[[i]][[j]]$PropOld <- result_adult[[i]][[j]]$PropOld[-(1:2000)]

    MeanILAdult[i,j] <- mean(result_adult[[i]][[j]]$PropIL)
    MeanOldAdult[i,j] <- mean(result_adult[[i]][[j]]$PropOld)
  }
}

OverallILAdult <- apply(MeanILAdult, 1, mean)
OverallOldAdult <- apply(MeanOldAdult, 1, mean)




####
###
##
# Plotting script
##
###
####


ProbInn <- 0.5  #Success rate of individual learning z
err <- 0        # Social Learning Error




library(fields)

par(mfrow = c(4,2),
    mar = c(2,2,1,0.5),
    oma = c(3,4,2,4))

for (bta in c(1,0.1,0.01, 0)) {
  

colors <- colorRampPalette(c("black", "darkred", "red","orange", "yellow", "lightyellow"))

z <- matrix(NA, nrow = length(unique(seq_adult$u)), ncol = length(unique(seq_adult$sigma)) )
for (i in unique(seq_adult$u)) {
  for (j in unique(seq_adult$sigma)) {
    z[which(unique(seq_adult$u)==i),  which(unique(seq_adult$sigma)==j)] <- mean(OverallILAdult[which(seq_adult$u==i & seq_adult$sigma==j & seq_adult$z == ProbInn & seq_adult$SL_Error == err & seq_adult$beta == bta) ])
    
  }
}

image(1:9, unique(seq_adult$sigma),1-z, col=colors(1000), zlim= c(0,1),xaxt="n", xlab="", ylab="")
axis(side=1, at=1:9, labels=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) 
if (bta == 1) mtext(side = 3, line = 1, "Proportion of Social Learners", cex = 1.1)
mtext(side = 3, line = -1.5, "", cex = 1.3, outer = TRUE)


if (bta == 1){
  mtext(side = 2, cex = 1, line = 2, expression(paste(beta," = 1" )   ))
  mtext(side = 2, cex = 0.7, line = 5,  "Mostly Juveniles Learn", font = 3 )   
} 
if (bta == 0.1) mtext(side = 2, cex = 1, line = 2, expression(paste(beta," = 0.1" )   ))
if (bta == 0.01) mtext(side = 2, cex = 1, line = 2, expression(paste(beta," = 0.01" )   ))
if (bta == 0) {
  mtext(side = 2, cex = 1, line = 2, expression(paste(beta," = 0") ))
  mtext(side = 2, cex = 0.7, line = 5,  "All Adults Learn", font = 3 )   
} 



z <- matrix(NA, nrow = length(unique(seq_adult$u)), ncol = length(unique(seq_adult$sigma)) )
for (i in unique(seq_adult$u)) {
  for (j in unique(seq_adult$sigma)) {
    z[which(unique(seq_adult$u)==i),  which(unique(seq_adult$sigma)==j)] <- mean(OverallOldAdult[which(seq_adult$u==i & seq_adult$sigma==j & seq_adult$z == ProbInn & seq_adult$SL_Error == err & seq_adult$beta == bta) ])
    
  }
}

image(1:9, unique(seq_adult$sigma),z, col=colors(1000), zlim= c(0,1),xaxt="n", xlab="", ylab="")
axis(side=1, at=1:9, labels=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) 
if (bta == 1) mtext(side = 3, line = 1, "Proportion of Old Bias", cex = 1.1)
mtext(side = 1, line = 2, expression(paste("Rate of environmental change ",italic("u"))), outer = TRUE, cex = 1.1)

}

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", c("1","", "0.9","", "0.8","", "0.7","","0.6","","0.5","","0.4","","0.3","","0.2","","0.1", "","0"), col= colors(1000)[rev(c(1,50,100,150,200,250, 300,350,400,450,500,550,600,650,700,750,800,850,900,950, 1000))], xpd = TRUE, inset = c(0, 0),bty="n", pch=15,cex = 1.2, pt.cex = 3.2)

mtext(side = 2, outer = TRUE, cex = 1.3, line = -1.5, expression(paste(sigma," = ", s[0]/s[1])))



