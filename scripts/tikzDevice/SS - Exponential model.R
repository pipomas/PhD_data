


x <- seq(.00001,10,.01)

# expFunction <- function(x,intercept,slope){I(intercept*exp(slope*x))}
round(mean(res$Sinter), digits = 0)
round(mean(res$Sslope), digits=3)
round(mean(res$Srsquared), digits =2)

round(summary(res$Sslope), digits=3)
round(sd(res$Sslope), digits=3)

y <- mean(res$Sinter)*exp(mean(res$Sslope)*x)

std <- function(x) sd(x)/sqrt(length(x))



# plot --------------------------------------------------------------------
plot.new()
par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 4.5), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7,0),      # 
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(x, y, lty = 1, pch = "", type = "o",
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     log = "xy",
     xlim = c(1,10),
     ylim = c(50,200),
     yaxt = "n",
     xaxt = "n",
     
     ylab = paste("Schwellensch{\"a}tzungen 82", "\\%", "korrekt (ms)"),
     xlab = paste("Mustergr{\"o}sse","($\\circ$)"),
     cex.lab=1.2,
     #cex.sub=3,
     col = "black",
     
     cex = 1,
     bty = "n",
     las = 1)

# axis(side = 1, at = c(1,10), las=1, cex.axis = 1.2, labels = FALSE)
axis(side = 1, at = c(1,1.8,3.6,5.4,7.2,10), labels = c("1", "1.8", "3.6", "5.4", "7.2", "10"), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(50,200,50), las=2, cex.axis = 1.2)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2))

points(1.8, mean(res$S1mean), cex = 1.5, pch = 20)
points(3.6, mean(res$S2mean), cex = 1.5, pch = 20)
points(5.4, mean(res$S3mean), cex = 1.5, pch = 20)
points(7.2, mean(res$S4mean), cex = 1.5, pch = 20)

# hack: we draw arrows but with very special "arrowheads"
std <- function(x) sd(x)/sqrt(length(x))
arrows(1.8, mean(res$S1mean)-std(res$S1mean), 1.8, mean(res$S1mean)+std(res$S1mean), length=0.05, angle=90, code=3)
arrows(3.6, mean(res$S2mean)-std(res$S2mean), 3.6, mean(res$S2mean)+std(res$S2mean), length=0.05, angle=90, code=3)
arrows(5.4, mean(res$S3mean)-std(res$S3mean), 5.4, mean(res$S3mean)+std(res$S3mean), length=0.05, angle=90, code=3)
arrows(7.2, mean(res$S4mean)-std(res$S4mean), 7.2, mean(res$S4mean)+std(res$S4mean), length=0.05, angle=90, code=3)


# tikz --------------------------------------------------------------------

tikz("output/tikz/expmodel.tex")
plot.new()
par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 4.5), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7,0),      # 
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(x, y, lty = 1, pch = "", type = "o",
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     log = "xy",
     xlim = c(1,10),
     ylim = c(50,200),
     yaxt = "n",
     xaxt = "n",
     
     ylab = paste("Schwellensch{\"a}tzungen 82", "\\%", "korrekt (ms)"),
     xlab = paste("Mustergr{\"o}sse","($^\\circ$)"),
     cex.lab=1.2,
     #cex.sub=3,
     col = "black",
     
     cex = 1,
     bty = "n",
     las = 1)

# axis(side = 1, at = c(1,10), las=1, cex.axis = 1.2, labels = FALSE)
axis(side = 1, at = c(1,1.8,3.6,5.4,7.2,10), labels = c("1", "1.8", "3.6", "5.4", "7.2", "10"), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(50,200,50), las=2, cex.axis = 1.2)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2))

points(1.8, mean(res$S1mean), cex = 1.5, pch = 20)
points(3.6, mean(res$S2mean), cex = 1.5, pch = 20)
points(5.4, mean(res$S3mean), cex = 1.5, pch = 20)
points(7.2, mean(res$S4mean), cex = 1.5, pch = 20)

# hack: we draw arrows but with very special "arrowheads"
arrows(1.8, mean(res$S1mean)-std(res$S1mean), 1.8, mean(res$S1mean)+std(res$S1mean), length=0.05, angle=90, code=3)
arrows(3.6, mean(res$S2mean)-std(res$S2mean), 3.6, mean(res$S2mean)+std(res$S2mean), length=0.05, angle=90, code=3)
arrows(5.4, mean(res$S3mean)-std(res$S3mean), 5.4, mean(res$S3mean)+std(res$S3mean), length=0.05, angle=90, code=3)
arrows(7.2, mean(res$S4mean)-std(res$S4mean), 7.2, mean(res$S4mean)+std(res$S4mean), length=0.05, angle=90, code=3)
dev.off()