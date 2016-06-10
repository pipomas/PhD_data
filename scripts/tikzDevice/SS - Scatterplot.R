library(tikzDevice)

# in R --------------------------------------------------------------------


par(mfrow = c(1, 4),       # 1x4 layout
    oma = c(0,0,0,0),    # two rows of text at the outer left and bottom margin
    mar = c(4.5, 5.1, 1.4, .6), # space for one row of text at ticks and to separate plots
    mgp = c(1.8, .7),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond)
    

plot(res$S1mean,
     main = "$1.8^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S1mean), col = "black", lwd = 1, lty=1)
title(ylab = paste("Schwellensch{\"a}tzungen 82", "\\%", "korrekt (ms)"), cex.lab = 2, line = 3.5)


plot(res$S2mean,
     main = "$3.6^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S2mean), col = "black", lwd = 1, lty=1)

plot(res$S3mean,
     main = "$5.4^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S3mean), col = "black", lwd = 1, lty=1)


plot(res$S4mean,
     main = "$7.2^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S4mean), col = "black", lwd = 1, lty=1)

par(mfrow=c(1,1))



# Generate .tex file ------------------------------------------------------
tikz("output/tikz/SS.tex")
par(mfrow = c(1, 4),       # 1x4 layout
    oma = c(0,0,0,0),    # two rows of text at the outer left and bottom margin
    mar = c(4.5, 5.1, 1.4, .6), # space for one row of text at ticks and to separate plots
    mgp = c(1.8, .7),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond)
    

plot(res$S1mean,
     main = "$1.8^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S1mean), col = "black", lwd = 1, lty=1)
title(ylab = paste("Schwellensch{\"a}tzungen 82", "\\%", "korrekt (ms)"), cex.lab = 2, line = 3.5)


plot(res$S2mean,
     main = "$3.6^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S2mean), col = "black", lwd = 1, lty=1)

plot(res$S3mean,
     main = "$5.4^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S3mean), col = "black", lwd = 1, lty=1)


plot(res$S4mean,
     main = "$7.2^\\circ$",
     cex.main=2,
     ylim = c(0,700),
     xlim = c(0,176),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,176), las=1, cex.axis = 1.5)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.5)
abline(h = mean(res$S4mean), col = "black", lwd = 1, lty=1)

par(mfrow=c(1,1))
 dev.off()
