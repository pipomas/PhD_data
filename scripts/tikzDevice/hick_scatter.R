# ---
# Title: "3_hick_scatter"
# Description: "This code produces a scatterplot of all reaction times in the hick task"
# Author: "Philipp Thomas"
# Date: "2016-07-11"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package

# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# set graphical parameters ----------
plot.new()

par(pty = "m",
    mfrow = c(1, 4),            # 1x4 layout
    mgp = c(2.5, .75, 0),        # axis label at 2 rows distance, tick labels at 1 row
    mai = c(.4, .4,.2,.1),       # space between plots
    xpd = FALSE)                # allow content to protrude into outer margin (and beyond)

plot(dat$H0meanRT,
     main = "\\textsf{0-bit}",
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
abline(h = mean(dat$H0meanRT), col = "black", lwd = 1, lty=1)
title(ylab = "Mittlere Reaktionszeit (ms)", cex.lab = 1.3, line = 2.5)


plot(dat$H1meanRT,
     main = "\\textsf{1-bit}",
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
abline(h = mean(dat$H1meanRT), col = "black", lwd = 1, lty=1)

plot(dat$H2meanRT,
     main = "\\textsf{2-bit}",
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
abline(h = mean(dat$H2meanRT), col = "black", lwd = 1, lty=1)

plot(dat$H258meanRT,
     main = "\\textsf{2.58-bit}",
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
abline(h = mean(dat$H258meanRT), col = "black", lwd = 1, lty=1)

par(mfrow=c(1,1))


# ==============================================================================
# Generate .tex file ------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/hick_scatterplot.tex",
     width = 5,
     height = 6)
plot.new()

par(pty = "m",
    mfrow = c(1, 4),            # 1x4 layout
    mgp = c(2.5, .75, 0),        # axis label at 2 rows distance, tick labels at 1 row
    mai = c(.4, .4,.2,.1),       # space between plots
    xpd = FALSE)                # allow content to protrude into outer margin (and beyond)

plot(dat$H0meanRT,
     main = "\\textsf{0-bit}",
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
abline(h = mean(dat$H0meanRT), col = "black", lwd = 1, lty=1)
title(ylab = "Mittlere Reaktionszeit (ms)", cex.lab = 1.3, line = 2.5)


plot(dat$H1meanRT,
     main = "\\textsf{1-bit}",
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
abline(h = mean(dat$H1meanRT), col = "black", lwd = 1, lty=1)

plot(dat$H2meanRT,
     main = "\\textsf{2-bit}",
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
abline(h = mean(dat$H2meanRT), col = "black", lwd = 1, lty=1)

plot(dat$H258meanRT,
     main = "\\textsf{2.58-bit}",
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
abline(h = mean(dat$H258meanRT), col = "black", lwd = 1, lty=1)

par(mfrow=c(1,1))

dev.off()