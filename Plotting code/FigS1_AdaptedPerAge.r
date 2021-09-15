#Plotting script for Fig. S1 Proportion adapted per age

#Function for proportion of adapted individuals conditional on age

Prob_Adapted <- function(i, u, s1, s0, p) ((1-u)^(i-1))* ((s1^(i-1))*p)/((s1^(i-1))*p + (s0^(i-1))*(1-p))

#Partial Derivative with respect to age, can be used to calculate maxima

DerivativeP_A <- function(i, u, s1, s0, p) { 
(1 - u)^(i) * p * s1^(i) * ((-(-1 + p) * s0^(i) + p * s1^(i)) * log(1 - u) + (-1 + p) * s0^(i) * (log(s0) - log(s1)))/
    ((-1 + p) * s0^(i) - p * s1^(i))^2

}


par(mfrow=c(2,3), 
    mar= c(1.5,1,1.5,2),
    oma= c(2,4,0,0))

for (u_seq in c(0.001, 0.01, 0.1)) {
  NEW <- ifelse(u_seq==0.001, FALSE, TRUE)
  curve(Prob_Adapted(x, u=u_seq, s1=0.8, s0=0.8, p=0.7), from = 1,lty=which(c(0.001,0.01, 0.1)==u_seq), to=80, ylim=c(0,1),lwd=1.5, add=NEW,xlab="Age", ylab = "P(adapted|Age)")
}
mtext(expression(paste(sigma," = 1")), side = 3, line = 0.5, cex = 0.7)
mtext(expression(paste(italic(z)," = 0.1                                           ", italic(z)," = 0.7")), side = 2, line = 2.9, outer = TRUE, cex = 0.7)
mtext("Proportion Adapted per Age", side = 2, line = 1.5, outer = TRUE, cex = 0.7)

mtext("Age", side = 1, line = 0.5, outer = TRUE, cex = 0.7)


for (u_seq in c(0.001, 0.01, 0.1)) {
  NEW <- ifelse(u_seq==0.001, FALSE, TRUE)
  curve(Prob_Adapted(x, u=u_seq, s1=0.8, s0=0.6, p=0.7), from = 1,lty=which(c(0.001,0.01, 0.1)==u_seq), to=80,ylim=c(0,1),lwd=1.5, add=NEW,xlab="Age", ylab = "P(adapted|Age)")
}
mtext(expression(paste(sigma," = 0.75")), side = 3, line = 0.5, cex = 0.7)

for (u_seq in c(0.001, 0.01, 0.1)) {
  NEW <- ifelse(u_seq==0.001, FALSE, TRUE)
  curve(Prob_Adapted(x, u=u_seq, s1=0.8, s0=0.4, p=0.7), from = 1,lty=which(c(0.001,0.01, 0.1)==u_seq), to=80, ylim=c(0,1),lwd=1.5, add=NEW,xlab="Age", ylab = "P(adapted|Age)")
}
mtext(expression(paste(sigma," = 0.5")), side = 3, line = 0.5, cex = 0.7)



for (u_seq in c(0.001, 0.01, 0.1)) {
  NEW <- ifelse(u_seq==0.001, FALSE, TRUE)
  curve(Prob_Adapted(x, u=u_seq, s1=0.8, s0=0.8, p=0.1), from = 1,lty=which(c(0.001,0.01, 0.1)==u_seq), to=80, ylim=c(0,1),lwd=1.5, add=NEW,xlab="Age", ylab = "P(adapted|Age)")
}
legend("top",title = expression(paste("Rate of Change ", italic(u))), c("0.001","0.01","0.1"), lty=1:5,lwd=1.5, cex = 1, bty="n")

for (u_seq in c(0.001, 0.01, 0.1)) {
  NEW <- ifelse(u_seq==0.001, FALSE, TRUE)
  curve(Prob_Adapted(x, u=u_seq,s1=0.8, s0=0.6, p=0.1), from = 1,lty=which(c(0.001,0.01, 0.1)==u_seq), to=80, ylim=c(0,1),lwd=1.5, add=NEW,xlab="Age", ylab = "P(adapted|Age)")
}

for (u_seq in c(0.001, 0.01, 0.1)) {
  NEW <- ifelse(u_seq==0.001, FALSE, TRUE)
  curve(Prob_Adapted(x, u=u_seq, s1=0.8, s0=0.4, p=0.1), from = 1,lty=which(c(0.001,0.01, 0.1)==u_seq), to=80, ylim=c(0,1),lwd=1.5, add=NEW,xlab="Age", ylab = "P(adapted|Age)")
}





