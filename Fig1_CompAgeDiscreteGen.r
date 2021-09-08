
# Code for Fig. 1 - Comparison of age-structured social learning model to
# version with discrete, non-overlapping generations

# Load function "recurs" in file "Recursions.R"

seq <-expand.grid(tmax=10000, max_age=c(2,80), u = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2),
                  z=1,c=c(0.01,0.05,0.1),phi = 1, SL_Error = 0, IL_Only = c("Yes", "No"), Stoch_E = "No")


library(parallel)

result <- mclapply(
  1:nrow(seq) ,
  function(i) recurs(seq$tmax[i], seq$max_age[i], b = c(1,1.5), s = c(0.9,0.9), seq$u[i], seq$z[i], seq$c[i], seq$phi[i], seq$SL_Error[i], seq$IL_Only[i], seq$Stoch_E[i]),
  mc.cores=72)



LambdaRog <- matrix(0, ncol = 6, nrow = 3)
LambdaRogIL <- matrix(0, ncol = 6, nrow = 3)
SLRog <- matrix(0, ncol = 6, nrow = 3)

LambdaAge <- matrix(0, ncol = 6, nrow = 3)
LambdaAgeIL <- matrix(0, ncol = 6, nrow = 3)
SLAge <- matrix(0, ncol = 6, nrow = 3)


for (i in unique(seq$c)) {
  for (j in unique(seq$u)) {
    
    LambdaRog[which(unique(seq$c) ==i), which(unique(seq$u) ==j)] <- result[[which(seq$c==i & seq$u==j & 
                                                                                     seq$max_age == 2 & seq$IL_Only == "No")]]$lambda[unique(seq$tmax)]
    
    LambdaRogIL[which(unique(seq$c) ==i), which(unique(seq$u) ==j)] <- result[[which(seq$c==i & seq$u==j & 
                                                                                       seq$max_age == 2 & seq$IL_Only == "Yes")]]$lambda[unique(seq$tmax)]
    
    
    SLRog[which(unique(seq$c) ==i), which(unique(seq$u) ==j)] <- result[[which(seq$c==i & seq$u==j & 
                                                                                 seq$max_age == 2 & seq$IL_Only == "No")]]$SL[unique(seq$tmax)]
    
    
    LambdaAge[which(unique(seq$c) ==i), which(unique(seq$u) ==j)] <- result[[which(seq$c==i & seq$u==j & 
                                                                                     seq$max_age == 80 & seq$IL_Only == "No")]]$lambda[unique(seq$tmax)]
    
    LambdaAgeIL[which(unique(seq$c) ==i), which(unique(seq$u) ==j)] <- result[[which(seq$c==i & seq$u==j & 
                                                                                       seq$max_age == 80 & seq$IL_Only == "Yes")]]$lambda[unique(seq$tmax)]
    
    
    SLAge[which(unique(seq$c) ==i), which(unique(seq$u) ==j)] <- result[[which(seq$c==i & seq$u==j & 
                                                                                 seq$max_age == 80 & seq$IL_Only == "No")]]$SL[unique(seq$tmax)]
  }
}



#color stuff
library(scales)
require(RColorBrewer)#load package
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values



graphics.off()
png("Drift.png", width = 17,height = 16, units = "cm", res = 900)


par(mfrow = c(2,2),
    mar = c(2,4,1,0.1),
    oma = c(2,0,2,0))


plot(SLRog[1,], lty=1, ylim = c(0,1),ann=FALSE, type = "l",xaxt= "n", xlab = "u", ylab = "Proportion Social Learners", lwd=3)
lines(SLRog[2,],lty = 2, lwd=3, col = "darkred")
lines(SLRog[3,], lty = 3, lwd=3, col = "orange")
axis(side = 1, at = 1:6, labels =  c("0.001", "0.005", "0.01", "0.05", "0.1", "0.2"))
mtext(side = 2, line = 2.5, "Proportion of Social Learners")
mtext(side = 3, line = 1, "Discrete Generations (Rogers Model)")
legend("bottomleft",title = expression(paste("Cost of Ind Learn ",italic("c"), ":")), c("0.01","0.05","0.1"), col = c("black", "darkred", "orange"), lty=1:3,lwd=3, cex =1, bty = "n")
legend("topright", "A", bty="n")

plot(SLAge[1,], lty=1, ylim = c(0,1), ann=FALSE, type = "l",xaxt= "n", xlab = "u", ylab = "Proportion Social Learners", lwd=3)
lines(SLAge[2,],lty = 2, lwd=3, col = "darkred")
lines(SLAge[3,], lty = 3, lwd=3, col = "orange")
axis(side = 1, at = 1:6, labels =  c("0.001", "0.005", "0.01", "0.05", "0.1", "0.2"))
mtext(side = 3, line = 1, "Overlapping Generations")
legend("topright", "B", bty="n")

plot(LambdaRog[1,], ylim = c(1.15,1.35), lty=1, col = "black",xaxt= "n", type = "l",xlab = "", ylab = "", lwd=3)
lines(LambdaRog[2,], lwd = 3, lty = 2, col = "darkred")
lines(LambdaRog[3,], lwd = 3, lty = 3, col = "orange")
axis(side = 1, at = 1:6, labels =  c("0.001", "0.005", "0.01", "0.05", "0.1", "0.2"))
mtext(side = 2, line = 2.5,  expression(paste("Fitness (Population Growth Rate  ",lambda, ")")))

points(1:6, LambdaRogIL[1,], pch=16, cex = 1.5, col = "grey")
points(1:6, LambdaRogIL[2,], pch=16, cex = 1.5, col = "grey")
points(1:6, LambdaRogIL[3,], pch=16, cex = 1.5, col = "grey")
legend("topright", "C", bty="n")

plot(LambdaAge[1,], ylim = c(2.05, 2.25), lty=1,xaxt= "n", col = "black", type = "l",xlab = "", ylab = "", lwd=3)
lines(LambdaAge[2,],lwd = 3, lty = 2, col = "darkred")
lines(LambdaAge[3,], lwd = 3, lty = 3, col = "orange")
axis(side = 1, at = 1:6, labels =  c("0.001", "0.005", "0.01", "0.05", "0.1", "0.2"))

points(1:6, LambdaAgeIL[1,], pch=16, cex = 1.5, col = "grey")
points(1:6, LambdaAgeIL[2,], pch=16, cex = 1.5, col = "grey")
points(1:6, LambdaAgeIL[3,], pch=16, cex = 1.5, col = "grey")
legend("topright", "D", bty="n")
mtext(side = 1, line = 1, outer=TRUE, expression(paste("Rate of environmental change ",italic("u"))))


dev.off()

