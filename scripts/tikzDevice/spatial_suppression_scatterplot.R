# ---
# Title: "spatial_suppression_scatterplot.R"
# Description: "This code reproduces a scatterplot of all thresholds in
# the spatial suppression task"
# Author: "Philipp Thomas"
# Date: "2016-09-01"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package

# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# set graphical parameters ---------
plot.new()
par(pty = "m",
    mfrow = c(1, 4),        # 1x4 layout
    mgp = c(2.5, .75, 0),   # axis label at 2 rows distance, tick labels at 1 row
    mai = c(.4, .4,.2,.1),  # space between plots
    xpd = FALSE)            # allow content to protrude into outer margin (and beyond)

plot(dat$S1mean,
     main = "\\textsf{1.8}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S1log10mean), col = "black", lwd = 1, lty=1)
title(ylab = "82\\,\\%-Erkennungsschwelle f{\"u}r horizontale Bewegung (ms)", cex.lab = 1.3, line = 2.5)


plot(dat$S2mean,
     main = "\\textsf{3.6}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S2log10mean), col = "black", lwd = 1, lty=1)

plot(dat$S3mean,
     main = "\\textsf{5.4}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S3log10mean), col = "black", lwd = 1, lty=1)

plot(dat$S4mean,
     main = "\\textsf{7.2}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S4log10mean), col = "black", lwd = 1, lty=1)

par(mfrow=c(1,1))




# ==============================================================================
# Generate .tex file ------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/spatial_suppression_scatterplot.tex",
     width = 5,
     height = 6)
plot.new()

par(pty = "m",
    mfrow = c(1, 4),            # 1x4 layout
    mgp = c(2.5, .75, 0),       # axis label at 2 rows distance, tick labels at 1 row
    mai = c(.4, .4,.2,.1),      # space between plots
    xpd = FALSE)                # allow content to protrude into outer margin (and beyond)

plot(dat$S1mean,
     main = "\\textsf{1.8}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S1log10mean), col = "black", lwd = 1, lty=1)
title(ylab = "82\\,\\%-Erkennungsschwelle f{\"u}r horizontale Bewegung (ms)", cex.lab = 1.3, line = 2.5)


plot(dat$S2mean,
     main = "\\textsf{3.6}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S2log10mean), col = "black", lwd = 1, lty=1)

plot(dat$S3mean,
     main = "\\textsf{5.4}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S3log10mean), col = "black", lwd = 1, lty=1)

plot(dat$S4mean,
     main = "\\textsf{7.2}$^\\circ$",
     cex.main=1.5,
     ylim = c(0,700),
     xlim = c(0,177),
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Vp",
     cex.lab=1.5,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(1,60,120,177), las=1, cex.axis = 1.04)
axis(side = 2, at = seq(0,700,100), las=2, cex.axis = 1.1)
abline(h = 10^mean(dat$S4log10mean), col = "black", lwd = 1, lty=1)

par(mfrow=c(1,1))

dev.off()
