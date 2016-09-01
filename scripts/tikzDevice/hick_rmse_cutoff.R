# ---
# Title: "hick_rmse_cutoff.R"
# Description: "This code reproduces line graphs for the reported RMSE cutoff
# analyses using the hick task"
# Author: "Philipp Thomas"
# Date: "2016-09-01"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package

# Relationship between Hick INTERCEPT and BIS Z SCORE --------------------------
# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# Generating data frame ----------------
mycolumns <- length(seq(3, 70, .1))
data <- data.frame(matrix(0, mycolumns, 6))
data <- rename(data,
               rmse = X1,
               n = X2,
               r = X3,
               lower = X4,
               upper = X5,
               p = X6)

for (i in 1:length(seq(3, 70, .1))){
  data[i, 1] <- seq(3, 70, .1)[i]
  data[i, 2] <- dat %>%
  filter(Hrmse <= data[i, 1]) %>%
  nrow()
  mycorrelation <- dat %>%
    filter(Hrmse <= data[i, 1]) %>%
    select(Hinter, zTotal) %>%
    corr.test()
  data[i, 3] <- mycorrelation$ci$r
  data[i, 4] <- mycorrelation$ci$lower
  data[i, 5] <- mycorrelation$ci$upper
  data[i, 6] <- mycorrelation$ci$p
}

# set graphical parameters ----------------------
dev.off()
par()
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 3),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(data$rmse, data$r,
     type = "n",
     ylim = c(-1, 1),
     xlim = c(0, 70),
     ylab = "\\textit{r}",
     xlab = "\\textit{RMSE}-Grenzwert (ms)",
     yaxt = "n",
     xaxt = "n",
     bty = "n")

axis(side = 1, at = c(3,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/hick_rmse_cutoff_intercept_zscore.tex",
     width = 5,
     height = 3)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 3),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(data$rmse, data$r,
     type = "n",
     ylim = c(-1, 1),
     xlim = c(0, 70),
     ylab = "\\textit{r}",
     xlab = "\\textit{RMSE}-Grenzwert (ms)",
     yaxt = "n",
     xaxt = "n",
     bty = "n")

axis(side = 1, at = c(3,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

dev.off()



# Relationship between Hick SLOPE and BIS Z SCORE --- --------------------------
# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# Generating data frame ----------------
mycolumns <- length(seq(3, 70, .1))
data <- data.frame(matrix(0, mycolumns, 6))
data <- rename(data,
               rmse = X1,
               n = X2,
               r = X3,
               lower = X4,
               upper = X5,
               p = X6)

for (i in 1:length(seq(3, 70, .1))){
  data[i, 1] <- seq(3, 70, .1)[i]
  data[i, 2] <- dat %>%
  filter(Hrmse <= data[i, 1]) %>%
  nrow()
  mycorrelation <- dat %>%
    filter(Hrmse <= data[i, 1]) %>%
    select(Hslope, zTotal) %>%
    corr.test()
  data[i, 3] <- mycorrelation$ci$r
  data[i, 4] <- mycorrelation$ci$lower
  data[i, 5] <- mycorrelation$ci$upper
  data[i, 6] <- mycorrelation$ci$p
}

# set graphical parameters ----------------------
dev.off()
par()
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 3),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(data$rmse, data$r,
     type = "n",
     ylim = c(-1, 1),
     xlim = c(0, 70),
     ylab = "\\textit{r}",
     xlab = "\\textit{RMSE}-Grenzwert (ms)",
     yaxt = "n",
     xaxt = "n",
     bty = "n")

axis(side = 1, at = c(3,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("-1.00","-.75","-.50","-.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/hick_rmse_cutoff_slope_zscore.tex",
     width = 5,
     height = 3)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),# space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),   # the margin line for the axis title, axis labels and axis line
    fin = c(5, 3),         # figure region dimensions in inch
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(data$rmse, data$r,
     type = "n",
     ylim = c(-1, 1),
     xlim = c(0, 70),
     ylab = "\\textit{r}",
     xlab = "\\textit{RMSE}-Grenzwert (ms)",
     yaxt = "n",
     xaxt = "n",
     bty = "n")

axis(side = 1, at = c(3,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

dev.off()
