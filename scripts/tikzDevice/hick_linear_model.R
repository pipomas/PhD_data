# ---
# Title: "hick_linear_model.R"
# Description: "This code reproduces a line graph of the linear model used to
# explain hick data"
# Author: "Philipp Thomas"
# Date: "2016-09-01"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package
# install.packages("plotrix")        # run this line if package is not installed yet
library(plotrix)                     # load package

# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# set graphical parameters -----
plot.new()
par(pty="s",               # make plot square
    oma = c(0, 0, 0, 0),   # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7, 0),     # the margin line for the axis title, axis labels and axis line
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

x <- seq(-1, 10, .01)                            # x values
y <- mean(dat$Hinter) + (mean(dat$Hslope) * x)   # y values

plot(x, y, lty = 1, pch = "", type = "o",
     xlim = c(0, log2(6)),
     ylim = c(150, 450),
     yaxt = "n",
     xaxt = "n",
     ylab = paste("Reaktionszeit (ms)"),
     xlab = "Bit ($\\log_{2}\\textnormal{n}$)",
     cex.lab = 1.2,
     col = "black",
     cex = 1,
     bty = "n",
     las = 1)

# x axis
axis(side = 1, at = c(log2(1),log2(2),log2(4),log2(6)), labels = c("0", "1", "2", "2.58"), las = 1, cex.axis = 1.2)

# y axis
axis(side = 2, at = seq(150, 450, 50), labels = c("0","200","250","300","350","400", "450"), las = 2, cex.axis = 1.2)
axis.break(axis = 2, breakpos = 175, style = "slash", brw = .015)

# mean points
points(log2(1), mean(dat$H0meanRT),   cex = 1.5, pch = 20)
points(log2(2), mean(dat$H1meanRT),   cex = 1.5, pch = 20)
points(log2(4), mean(dat$H2meanRT),   cex = 1.5, pch = 20)
points(log2(6), mean(dat$H258meanRT), cex = 1.5, pch = 20)

# hack: we draw arrows but with very special "arrowheads"
std <- function(x) sd(x)/sqrt(length(x))
arrows(log2(1), mean(dat$H0meanRT)-std(dat$H0meanRT),    log2(1), mean(dat$H0meanRT)+std(dat$H0meanRT), length=0.05, angle=90, code=3)
arrows(log2(2), mean(dat$H1meanRT)-std(dat$H1meanRT),    log2(2), mean(dat$H1meanRT)+std(dat$H1meanRT), length=0.05, angle=90, code=3)
arrows(log2(4), mean(dat$H2meanRT)-std(dat$H2meanRT),     log2(4), mean(dat$H2meanRT)+std(dat$H2meanRT), length=0.05, angle=90, code=3)
arrows(log2(6), mean(dat$H258meanRT)-std(dat$H258meanRT), log2(6), mean(dat$H258meanRT)+std(dat$H258meanRT), length=0.05, angle=90, code=3)


# Generate .tex file -----------------------------------------------------------
tikz("../Arbeit/PhD_thesis/tikzDevice/hick_linear_model.tex")
plot.new()
par(pty="s",               # make plot square
    oma = c(0, 0, 0, 0),   # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7, 0),     # the margin line for the axis title, axis labels and axis line
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

x <- seq(-1, 10, .01)                            # x values
y <- mean(dat$Hinter) + (mean(dat$Hslope) * x)   # y values

plot(x, y, lty = 1, pch = "", type = "o",
     xlim = c(0, log2(6)),
     ylim = c(150, 450),
     yaxt = "n",
     xaxt = "n",
     ylab = paste("Reaktionszeit (ms)"),
     xlab = "Bit ($\\log_{2}\\textnormal{n}$)",
     cex.lab = 1.2,
     col = "black",
     cex = 1,
     bty = "n",
     las = 1)

# x axis
axis(side = 1, at = c(log2(1),log2(2),log2(4),log2(6)), labels = c("0", "1", "2", "2.58"), las = 1, cex.axis = 1.2)

# y axis
axis(side = 2, at = seq(150, 450, 50), labels = c("0","200","250","300","350","400", "450"), las = 2, cex.axis = 1.2)
axis.break(axis = 2, breakpos = 175, style = "slash", brw = .015)

# mean points
points(log2(1), mean(dat$H0meanRT),   cex = 1.5, pch = 20)
points(log2(2), mean(dat$H1meanRT),   cex = 1.5, pch = 20)
points(log2(4), mean(dat$H2meanRT),   cex = 1.5, pch = 20)
points(log2(6), mean(dat$H258meanRT), cex = 1.5, pch = 20)

# hack: we draw arrows but with very special "arrowheads"
std <- function(x) sd(x)/sqrt(length(x))
arrows(log2(1), mean(dat$H0meanRT)-std(dat$H0meanRT),    log2(1), mean(dat$H0meanRT)+std(dat$H0meanRT), length=0.05, angle=90, code=3)
arrows(log2(2), mean(dat$H1meanRT)-std(dat$H1meanRT),    log2(2), mean(dat$H1meanRT)+std(dat$H1meanRT), length=0.05, angle=90, code=3)
arrows(log2(4), mean(dat$H2meanRT)-std(dat$H2meanRT),     log2(4), mean(dat$H2meanRT)+std(dat$H2meanRT), length=0.05, angle=90, code=3)
arrows(log2(6), mean(dat$H258meanRT)-std(dat$H258meanRT), log2(6), mean(dat$H258meanRT)+std(dat$H258meanRT), length=0.05, angle=90, code=3)

dev.off()