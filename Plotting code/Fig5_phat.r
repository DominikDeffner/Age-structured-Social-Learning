

# Plotting code for Fig. S3. Relationship between p-hat and old bias

# Plots results from "sim_temporal" in file "ABM_Simulations.r"



# Remove first 2000 timesteps and calculate mean for each parameter combination

MeanOld <- matrix(NA, nrow = nrow(seq_temporal), ncol = 10 )
p <- matrix(NA, nrow = nrow(seq_temporal), ncol = 10 )

for (i in 1:nrow(seq_temporal)){
  for (j in 1:10) {
    result_temporal[[i]][[j]]$PropOld <- result_temporal[[i]][[j]]$PropOld[-(1:2000)]
    result_temporal[[i]][[j]]$p <- result_temporal[[i]][[j]]$p[-(1:2000)]
    
    MeanOld[i,j] <- mean(result_temporal[[i]][[j]]$PropOld)
    p[i,j] <-  mean(result_temporal[[i]][[j]]$p)
  }
}

OverallOld <- apply(MeanOld, 1, mean)
Overallp <- apply(p, 1, mean)



####
###
##
# Plotting code
##
###
####


#color stuff
require(RColorBrewer)#load package
require(scales)
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values



par(mfrow=c(1,3),
    oma=c(2.5,1.4,0,0),
    mar=c(1.5,2.4,1.5,0.1))

x <- which(seq_temporal$u==0.001 & seq_temporal$sigma == 0.90)
plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=15, col=alpha(col.pal[1], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[1], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.001 & seq_temporal$sigma == 0.80)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=16, col=alpha(col.pal[3], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[3], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.001 & seq_temporal$sigma == 0.70)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=17, col=alpha(col.pal[6], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[6], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.001 & seq_temporal$sigma == 0.60)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=18, col=alpha(col.pal[4], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[4], lwd = 2)
 abline(h=0.5, lty=2, col="lightgrey")
 
 
 title(expression(paste("High Stability (", italic(u), " = 0.001)")))

 
 x <- which(seq_temporal$u==0.01 & seq_temporal$sigma == 0.90)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=15, col=alpha(col.pal[1], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[1], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.01 & seq_temporal$sigma == 0.80)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=16, col=alpha(col.pal[3], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[3], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.01 & seq_temporal$sigma == 0.70)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=17, col=alpha(col.pal[6], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[6], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.01 & seq_temporal$sigma == 0.60)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=18, col=alpha(col.pal[4], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[4], lwd = 2)
 
 abline(h=0.5, lty=2, col="lightgrey")
 
 title(expression(paste("Moderate Stability (", italic(u), " = 0.01)")))
 
 x <- which(seq_temporal$u==0.1 & seq_temporal$sigma == 0.90)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=15, col=alpha(col.pal[1], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[1], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.1 & seq_temporal$sigma == 0.80)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=16, col=alpha(col.pal[3], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[3], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.1 & seq_temporal$sigma == 0.70)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=17, col=alpha(col.pal[6], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[6], lwd = 2)
 
 par(new=TRUE)
 x <- which(seq_temporal$u==0.1 & seq_temporal$sigma == 0.60)
 plot(Overallp[x], OverallOld[x],ylim = c(0,1), xlim = c(0,1), pch=18, col=alpha(col.pal[4], alpha = 0.5))
 abline(lm(OverallOld[x]~Overallp[x]), col=col.pal[4], lwd = 2)
 
 abline(h=0.5, lty=2, col="lightgrey")
 
 title(expression(paste("Low Stability (", italic(u), " = 0.1)")))
 
 legend("topright", c(expression(paste(sigma," = 0.9")),expression(paste(sigma," = 0.8")),expression(paste(sigma," = 0.7")),
                      expression(paste(sigma," = 0.6"))), col = c(alpha(col.pal[1], alpha = 0.5),
                      alpha(col.pal[3], alpha = 0.5), alpha(col.pal[6], alpha = 0.5),alpha(col.pal[4], alpha = 0.5)), pch=c(15,16,17,18), lty=0, bty="n", cex = 1.2)
 
 mtext(side = 1, expression(paste(italic(hat(p))," (probability juvenile acquires adaptive behavior)")), line = 1.6, outer = TRUE, cex = 0.9)
 mtext(side = 2, "Proportion of Old Bias", line = 0, outer = TRUE, cex = 0.9)
 
 
 