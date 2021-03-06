# ---
# Title: "hick_rmse_density.R"
# Description: "This code reproduces the density function of the Hick RMSE"
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
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 2),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

rmse.density <- density(dat$Hrmse, bw=.7)
plot(rmse.density,
     main = "",
     xlim = c(0,70),
     ylim = c(0,.15),
     xaxt = "n",
     yaxt = "n",
     xlab = "\\textit{RMSE} (ms)",
     ylab = "Dichte",
     col = 1,
     bty = "n",
     zero.line = FALSE,
     )

axis(side = 1, at = seq(0,70,10))
axis(side = 2, at = seq(0,.15,.05), las = 2)

rug(dat$Hrmse, ticksize = .1)

# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/hick_rmse_density.tex",
     width = 5,
     height = 2)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 2),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

rmse.density <- density(dat$Hrmse, bw=.7)
plot(rmse.density,
     main = "",
     xlim = c(0,70),
     ylim = c(0,.15),
     xaxt = "n",
     yaxt = "n",
     xlab = "\\textit{RMSE} (ms)",
     ylab = "Dichte",
     col = 1,
     bty = "n",
     zero.line = FALSE,
     )

axis(side = 1, at = seq(0,70,10))
axis(side = 2, at = seq(0,.15,.05), las = 2)

rug(dat$Hrmse, ticksize = .1)

dev.off()