
# Code for Fig. 3 Thresholds for older generations
# to have higher proportions of adaptive behavior 

p <- function(u, s){
  1 - (u / (1 - s))
}

par(oma=c(0,0,0,0))
curve(p(u=0.01,x), from = 0, to = 1, type = "n", bty = "l",ylab = "", xlab = "")
for (u in c(0.001,0.01,0.1,0.2,0.3,0.4, 0.5)) {
  curve(p(u=u,x), from = 0, to = 1, add = TRUE, lwd = 1.5, ylab = "", xlab = "")
}
mtext(expression(italic(hat(p))), line=2.5, side=2, cex = 1.2)
mtext(expression(paste(sigma," = ", s[0]/s[1])), line=3, side=1, cex = 1.2)

text(0.26,0.26, expression(paste(italic(u), " = 0.5")), srt = -45, cex = 0.7)
text(0.34,0.34, expression(paste(italic(u), " = 0.4")), srt = -45, cex = 0.7)

text(0.42,0.42, expression(paste(italic(u), " = 0.3")), srt = -45, cex = 0.7)

text(0.52,0.52, expression(paste(italic(u), " = 0.2")), srt = -45, cex = 0.7)
text(0.65,0.65,  expression(paste(italic(u), " = 0.1")), srt = -45, cex = 0.7)
text(0.86,0.86,  expression(paste(italic(u), " = 0.01")), srt = -44, cex = 0.7)
text(0.914,0.914,  expression(paste(italic(u), " = 0.001")), srt = -45, cex = 0.7)
