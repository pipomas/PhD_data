# ---
# Title: "spatial_suppression_exponential_model.R"
# Description: "This code reproduces a line graph of the linear model used to
# explain spatial suppression data"
# Author: "Philipp Thomas"
# Date: "2016-09-01"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package
# install.packages("plotrix")     # run this line if package is not installed yet
library(plotrix)

# this stuff is needed for plotting -------------------------
std <- function(x) sd(x)/sqrt(length(x))
x <- seq(.00001,10,.01)
y <- mean(dat$Sasymptote)*exp(mean(dat$Sslope)*x)

line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}


# Write code before generating .tex file ---------------------------------------
# ==============================================================================

# set graphical parameters --------------
plot.new()
par(pty="s",               # make plot square
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7,0),      # the margin line for the axis title, axis labels and axis line
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

x <- seq(.00001,10,.01)
y <- 70 * exp(.103 * x)

plot(x, y, lty = 1, pch = "", type = "o",
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     log = "xy",
     xlim = c(1,10),
     ylim = c(40,200),
     axes = FALSE,
     ylab = paste("82\\,\\%-Erkennungsschwelle f{\"u}r horizontale Bewegung (ms)"),
     xlab = paste("Mustergr{\"o}sse","($^\\circ$)"),
     cex.lab=1.2,
     col = "black",
     cex = 1,
     bty = "n",
     las = 1)

# x axis
axis(side = 1, at = c(1.8,3.6,5.4,7.2), las=1, cex.axis = 1.2)
lines(x = c(1, 10), y = rep(line2user(0, side = 1), 2), xpd = TRUE)

# y axis
axis(side = 2, at = c(40,50,100,150,200), labels = c("0","50","100","150","200"), las=2, cex.axis = 1.2)
axis.break(axis = 2, breakpos=45, style="slash", brw = .015)


# (70 * exp(.103 * 1.8))

# # "mean" points (we use the inverted values here - they do not correspond
# # with the means reported in text and tables)
points(1.8, (70 * exp(.103 * 1.8)), cex = 1.5, pch = 20)
points(3.6, (70 * exp(.103 * 3.6)), cex = 1.5, pch = 20)
points(5.4, (70 * exp(.103 * 5.4)), cex = 1.5, pch = 20)
points(7.2, (70 * exp(.103 * 7.2)), cex = 1.5, pch = 20)



x <- seq(.00001,10,.01)
y <- 50 * exp(.103 * x)

lines(x, y, lty = 2)

# # x axis
# axis(side = 1, at = c(1.8,3.6,5.4,7.2), las=1, cex.axis = 1.2)
# lines(x = c(1, 10), y = rep(line2user(0, side = 1), 2), xpd = TRUE)
#
# # y axis
# axis(side = 2, at = c(40,50,100,150,200), labels = c("0","50","100","150","200"), las=2, cex.axis = 1.2)
# axis.break(axis = 2, breakpos=45, style="slash", brw = .015)


# (70 * exp(.103 * 1.8))

# # "mean" points (we use the inverted values here - they do not correspond
# # with the means reported in text and tables)
points(1.8, (50 * exp(.103 * 1.8)), cex = 1.5, pch = 18)
points(3.6, (50 * exp(.103 * 3.6)), cex = 1.5, pch = 18)
points(5.4, (50 * exp(.103 * 5.4)), cex = 1.5, pch = 18)
points(7.2, (50 * exp(.103 * 7.2)), cex = 1.5, pch = 18)



# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz("../Arbeit/PhD_thesis/tikzDevice/spatial_suppression_scale_figure.tex")
plot.new()
# set graphical parameters --------------
plot.new()
par(pty="s",               # make plot square
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7,0),      # the margin line for the axis title, axis labels and axis line
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

x <- seq(.00001,10,.01)
y <- 70 * exp(.103 * x)

plot(x, y, lty = 1, pch = "", type = "o",
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     log = "xy",
     xlim = c(1,10),
     ylim = c(40,200),
     axes = FALSE,
     ylab = paste("82\\,\\%-Erkennungsschwelle f{\"u}r horizontale Bewegung (ms)"),
     xlab = paste("Mustergr{\"o}sse","($^\\circ$)"),
     cex.lab=1.2,
     col = "black",
     cex = 1,
     bty = "n",
     las = 1)

# x axis
axis(side = 1, at = c(1.8,3.6,5.4,7.2), las=1, cex.axis = 1.2)
lines(x = c(1, 10), y = rep(line2user(0, side = 1), 2), xpd = TRUE)

# y axis
axis(side = 2, at = c(40,50,100,150,200), labels = c("0","50","100","150","200"), las=2, cex.axis = 1.2)
axis.break(axis = 2, breakpos=45, style="slash", brw = .015)


# (70 * exp(.103 * 1.8))

# # "mean" points (we use the inverted values here - they do not correspond
# # with the means reported in text and tables)
points(1.8, (70 * exp(.103 * 1.8)), cex = 1.5, pch = 20)
points(3.6, (70 * exp(.103 * 3.6)), cex = 1.5, pch = 20)
points(5.4, (70 * exp(.103 * 5.4)), cex = 1.5, pch = 20)
points(7.2, (70 * exp(.103 * 7.2)), cex = 1.5, pch = 20)



x <- seq(.00001,10,.01)
y <- 50 * exp(.103 * x)

lines(x, y, lty = 2)

# # x axis
# axis(side = 1, at = c(1.8,3.6,5.4,7.2), las=1, cex.axis = 1.2)
# lines(x = c(1, 10), y = rep(line2user(0, side = 1), 2), xpd = TRUE)
#
# # y axis
# axis(side = 2, at = c(40,50,100,150,200), labels = c("0","50","100","150","200"), las=2, cex.axis = 1.2)
# axis.break(axis = 2, breakpos=45, style="slash", brw = .015)


# (70 * exp(.103 * 1.8))

# # "mean" points (we use the inverted values here - they do not correspond
# # with the means reported in text and tables)
points(1.8, (50 * exp(.103 * 1.8)), cex = 1.5, pch = 18)
points(3.6, (50 * exp(.103 * 3.6)), cex = 1.5, pch = 18)
points(5.4, (50 * exp(.103 * 5.4)), cex = 1.5, pch = 18)
points(7.2, (50 * exp(.103 * 7.2)), cex = 1.5, pch = 18)

dev.off()