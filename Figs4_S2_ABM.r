
# Plotting code for Figs. 4 and S2 (heatmaps). Proportions of social learners and old bias
# for the temporal and spatial individual-based model.

# Plots results from "ABM_Simulations.r"

# Remove first 2000 timesteps and calculate mean for each parameter combination

# Temporal Model

MeanILTemp <- matrix(NA, nrow = nrow(seq_temporal), ncol = 10 )
MeanOldTemp <- matrix(NA, nrow = nrow(seq_temporal), ncol = 10 )

for (i in 1:nrow(seq_temporal)){
  for (j in 1:10) {
    result_temporal[[i]][[j]]$PropIL <- result_temporal[[i]][[j]]$PropIL[-(1:2000)]
    result_temporal[[i]][[j]]$PropOld <- result_temporal[[i]][[j]]$PropOld[-(1:2000)]

    MeanILTemp[i,j] <- mean(result_temporal[[i]][[j]]$PropIL)
    MeanOldTemp[i,j] <- mean(result_temporal[[i]][[j]]$PropOld)
  }
}

OverallILTemp <- apply(MeanILTemp, 1, mean)
OverallOldTemp <- apply(MeanOldTemp, 1, mean)


# Spatial Model

MeanILSpat <- matrix(NA, nrow = nrow(seq_spatial), ncol = 10 )
MeanOldSpat <- matrix(NA, nrow = nrow(seq_spatial), ncol = 10 )

for (i in 1:nrow(seq_spatial)){
  for (j in 1:10) {
    result_spatial[[i]][[j]]$PropIL <- result_spatial[[i]][[j]]$PropIL[-(1:2000)]
    result_spatial[[i]][[j]]$PropOld <- result_spatial[[i]][[j]]$PropOld[-(1:2000)]
    
    MeanILSpat[i,j] <- mean(result_spatial[[i]][[j]]$PropIL)
    MeanOldSpat[i,j] <- mean(result_spatial[[i]][[j]]$PropOld)
  }
}

OverallILSpat <- apply(MeanILSpat, 1, mean)
OverallOldSpat <- apply(MeanOldSpat, 1, mean)



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

par(mfrow = c(2,2),
    mar = c(2,3,1,0),
    oma = c(2,3,2,4))

colors <- colorRampPalette(c("black", "darkred", "red","orange", "yellow", "lightyellow"))

z <- matrix(NA, nrow = length(unique(seq_temporal$u)), ncol = length(unique(seq_temporal$sigma)) )
for (i in unique(seq_temporal$u)) {
  for (j in unique(seq_temporal$sigma)) {
    z[which(unique(seq_temporal$u)==i),  which(unique(seq_temporal$sigma)==j)] <- mean(OverallILTemp[which(seq_temporal$u==i & seq_temporal$sigma==j & seq_temporal$z == ProbInn & seq_temporal$SL_Error == err) ])
    
  }
}

image(1:9, unique(seq_temporal$sigma),1-z, col=colors(1000), zlim= c(0,1),xaxt="n", xlab="", ylab="")
axis(side=1, at=1:9, labels=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) 
mtext(side = 2, line = 5, "Proportion of Social Learners")
mtext(side = 3, line = 1, "Temporal Model", cex = 1.3)


z <- matrix(NA, nrow = length(unique(seq_spatial$m)), ncol = length(unique(seq_spatial$sigma)) )
for (i in unique(seq_spatial$m)) {
  for (j in unique(seq_spatial$sigma)) {
    z[which(unique(seq_spatial$m)==i),  which(unique(seq_spatial$sigma)==j)] <- mean(OverallILSpat[which(seq_spatial$m==i & seq_spatial$sigma==j & seq_spatial$z == ProbInn & seq_spatial$SL_Error == err) ])
    
  }
}

image(1:9, unique(seq_spatial$sigma),1-z, col=colors(1000), zlim= c(0,1),xaxt="n", xlab="", ylab="")
axis(side=1, at=1:9, labels=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) 
mtext(side = 3, line = 1, "Spatial Model", cex = 1.3)


z <- matrix(NA, nrow = length(unique(seq_temporal$u)), ncol = length(unique(seq_temporal$sigma)) )
for (i in unique(seq_temporal$u)) {
  for (j in unique(seq_temporal$sigma)) {
    z[which(unique(seq_temporal$u)==i),  which(unique(seq_temporal$sigma)==j)] <- mean(OverallOldTemp[which(seq_temporal$u==i & seq_temporal$sigma==j & seq_temporal$z == ProbInn & seq_temporal$SL_Error == err) ])
    
  }
}

image(1:9, unique(seq_temporal$sigma),z, col=colors(1000), zlim= c(0,1),xaxt="n", xlab="", ylab="")
axis(side=1, at=1:9, labels=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) 
mtext(side = 2, line = 5, "Proportion of Old Bias")
mtext(side = 2, outer = TRUE, cex = 1.25, line = -0.2, expression(paste(sigma," = ", s[0]/s[1])))
mtext(side = 1, line = 3, expression(paste("Rate of environmental change ",italic("u"))))


z <- matrix(NA, nrow = length(unique(seq_spatial$m)), ncol = length(unique(seq_spatial$sigma)) )
for (i in unique(seq_spatial$m)) {
  for (j in unique(seq_spatial$sigma)) {
    z[which(unique(seq_spatial$m)==i),  which(unique(seq_spatial$sigma)==j)] <- mean(OverallOldSpat[which(seq_spatial$m==i & seq_spatial$sigma==j & seq_spatial$z == ProbInn & seq_spatial$SL_Error == err) ])
    
  }
}

image(1:9, unique(seq_spatial$sigma),z, col=colors(1000), zlim= c(0,1),xaxt="n", xlab="", ylab="")
axis(side=1, at=1:9, labels=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) 
mtext(side = 1, line = 3, expression(paste("Migration Rate ",italic("m"))))

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", c("1","", "0.9","", "0.8","", "0.7","","0.6","","0.5","","0.4","","0.3","","0.2","","0.1", "","0"), col= colors(1000)[rev(c(1,50,100,150,200,250, 300,350,400,450,500,550,600,650,700,750,800,850,900,950, 1000))], xpd = TRUE, inset = c(0, 0),bty="n", pch=15,cex = 1.2, pt.cex = 3.2)





