# ---
# Title: "bis_density.R"
# Description: "This code reproduces the density function of the BIS z score"
# Author: "Philipp Thomas"
# Date: "2016-09-01"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package

# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# set graphical parameters ----------------------
dev.off()
par()
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0, 0, 0, 0),   # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 2),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

bis.density <- density(dat$zTotal, bw = .08)
plot(bis.density,
     main = "",
     xlim = c(-2, 2),
     ylim = c(0, 1),
     xaxt = "n",
     yaxt = "n",
     xlab = "BIS-Test \\textit{z}-Wert",
     ylab = "Dichte",
     freq = TRUE,
     col = 1,
     border = NA,
     bty = "n",
     zero.line = FALSE)

axis(side = 1, at = seq(-2, 2, 1))
axis(side = 2, at = seq(0, 1, .25), las = 2)

rug(dat$zTotal, ticksize = .1)

# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/bis_density.tex",
     width = 5,
     height = 2)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0, 0, 0, 0),   # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line.
    fin = c(5, 2),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

bis.density <- density(dat$zTotal, bw = .08)
plot(bis.density,
     main = "",
     xlim = c(-2, 2),
     ylim = c(0, 1),
     xaxt = "n",
     yaxt = "n",
     xlab = "BIS-Test \\textit{z}-Wert",
     ylab = "Dichte",
     freq = TRUE,
     col = 1,
     border = NA,
     bty = "n",
     zero.line = FALSE)

axis(side = 1, at = seq(-2, 2, 1))
axis(side = 2, at = seq(0, 1, .25), las = 2)

rug(dat$zTotal, ticksize = .1)

dev.off()