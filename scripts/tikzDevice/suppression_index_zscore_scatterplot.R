# ---
# Title: "suppression_index_zscore_scatterplot.R"
# Description: "This code produces a scatterplot of the relationship between the
# suppression index and the BIS z score"
# Author: "Philipp Thomas"
# Date: "2016-09-01"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package

# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# set graphical parameters ----------
par(pty="s",               # make plot square
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4.1, 4, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(1.8, .7),      # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(dat$Si, dat$zTotal,
     xlim = c(-.4,1),
     ylim = c(-2,2),
     yaxt = "n",
     xaxt = "n",
     ylab = "BIS-Test z-Wert",
     xlab = "Suppression-Index",
     cex.lab=1.2,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

# abline(a = lm(dat$zTotal ~ dat$si)$coefficients[1],
#        b = lm(dat$zTotal ~ dat$si)$coefficients[2],
#        lty = 2)

axis(side = 1, at = seq(-.4,1,.20), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(-2,2,1), las=2, cex.axis = 1.2)


# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz("../Arbeit/PhD_thesis/tikzDevice/si_intelligence_scatterplot.tex")

par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4.1, 4, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(1.8, .7),      # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)

plot(dat$si, dat$zTotal,
     xlim = c(-.4,1),
     ylim = c(-2,2),
     yaxt = "n",
     xaxt = "n",
     ylab = "BIS-Test \\textit{z}-Wert",
     xlab = "Suppression-Index",
     cex.lab=1.2,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

# abline(a = lm(dat$zTotal ~ dat$si)$coefficients[1],
#        b = lm(dat$zTotal ~ dat$si)$coefficients[2],
#        lty = 2)

axis(side = 1, at = seq(-.4,1,.20), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(-2,2,1), las=2, cex.axis = 1.2)

par(mfrow=c(1,1))

dev.off()
