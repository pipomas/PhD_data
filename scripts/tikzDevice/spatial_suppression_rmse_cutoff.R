# ---
# Title: "spatial_suppression_rmse_cutoff.R"
# Description: "This code produces a histogram of the suppression index"
# Author: "Philipp Thomas"
# Date: "2016-07-11"
# ---

# install.packages("tikzDevice")     # run this line if package is not installed yet
library(tikzDevice)                  # load package

# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# Generating data frame for correlation between ASYMPTOTE and SLOPE ------------
mycolumns <- length(seq(1, 70, .1))
data <- data.frame(matrix(0, mycolumns, 6))
data <- rename(data,
               rmse = X1,
               n = X2,
               r = X3,
               lower = X4,
               upper = X5,
               p = X6)

for (i in 1:length(seq(1, 70, .1))){
  data[i, 1] <- seq(1, 70, .1)[i]
  data[i, 2] <- dat %>%
  filter(Srmse <= data[i, 1]) %>%
  nrow()
  mycorrelation <- dat %>%
    filter(Srmse <= data[i, 1]) %>%
    select(Sasymptote, Sslope) %>%
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
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

# par(fig=c(0.65,1,0,0.8),new=TRUE)
# boxplot(mtcars$mpg, axes=FALSE)

# boxplot(dat$Srmse,
#         horizontal=TRUE,
#         outline=TRUE,
#         frame=F,
#         axes = FALSE,
#         # notch = TRUE,
#         add = TRUE,
#         range = 1,
#         pch = 20,
#         at = -.75,
#         lty = 1)

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)


# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/spatial_suppression_rmse_cutoff_asymptote_slope.tex",
     width = 5,
     height = 3)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

dev.off()




# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# Generating data frame for correlation between SLOPE and SUPPRESSION-INDEX ----
mycolumns <- length(seq(1, 70, .1))
data <- data.frame(matrix(0, mycolumns, 6))
data <- rename(data,
               rmse = X1,
               n = X2,
               r = X3,
               lower = X4,
               upper = X5,
               p = X6)

for (i in 1:length(seq(1, 70, .1))){
  data[i, 1] <- seq(1, 70, .1)[i]
  data[i, 2] <- dat %>%
  filter(Srmse <= data[i, 1]) %>%
  nrow()
  mycorrelation <- dat %>%
    filter(Srmse <= data[i, 1]) %>%
    select(Sslope, si) %>%
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
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

# par(fig=c(0.65,1,0,0.8),new=TRUE)
# boxplot(mtcars$mpg, axes=FALSE)

# boxplot(dat$Srmse,
#         horizontal=TRUE,
#         outline=TRUE,
#         frame=F,
#         axes = FALSE,
#         # notch = TRUE,
#         add = TRUE,
#         range = 1,
#         pch = 20,
#         at = -.75,
#         lty = 1)

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)


# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/spatial_suppression_rmse_cutoff_slope_suppressionindex.tex",
     width = 5,
     height = 3)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

dev.off()



# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# Generating data frame for correlation between ASYMPTOTE and Z SCORE ----------
mycolumns <- length(seq(1, 70, .1))
data <- data.frame(matrix(0, mycolumns, 6))
data <- rename(data,
               rmse = X1,
               n = X2,
               r = X3,
               lower = X4,
               upper = X5,
               p = X6)

for (i in 1:length(seq(1, 70, .1))){
  data[i, 1] <- seq(1, 70, .1)[i]
  data[i, 2] <- dat %>%
  filter(Srmse <= data[i, 1]) %>%
  nrow()
  mycorrelation <- dat %>%
    filter(Srmse <= data[i, 1]) %>%
    select(Sasymptote, zTotal) %>%
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
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

# par(fig=c(0.65,1,0,0.8),new=TRUE)
# boxplot(mtcars$mpg, axes=FALSE)

# boxplot(dat$Srmse,
#         horizontal=TRUE,
#         outline=TRUE,
#         frame=F,
#         axes = FALSE,
#         # notch = TRUE,
#         add = TRUE,
#         range = 1,
#         pch = 20,
#         at = -.75,
#         lty = 1)

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/spatial_suppression_rmse_cutoff_asymptote_zscore.tex",
     width = 5,
     height = 3)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

dev.off()




# Write code before generating .tex file ---------------------------------------
# ==============================================================================
# Generating data frame for correlation between SLOPE and Z SCORE --------------
mycolumns <- length(seq(1, 70, .1))
data <- data.frame(matrix(0, mycolumns, 6))
data <- rename(data,
               rmse = X1,
               n = X2,
               r = X3,
               lower = X4,
               upper = X5,
               p = X6)

for (i in 1:length(seq(1, 70, .1))){
  data[i, 1] <- seq(1, 70, .1)[i]
  data[i, 2] <- dat %>%
  filter(Srmse <= data[i, 1]) %>%
  nrow()
  mycorrelation <- dat %>%
    filter(Srmse <= data[i, 1]) %>%
    select(Sslope, zTotal) %>%
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
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("-1.00","-.75","-.50","-.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

# ==============================================================================
# Generate .tex file -----------------------------------------------------------
tikz(file = "../Arbeit/PhD_thesis/tikzDevice/spatial_suppression_rmse_cutoff_slope_zscore.tex",
     width = 5,
     height = 3)
plot.new()

par(pty="m",               # vs "s" (= square). Maximum plotting region ("m")
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 3.5, .5, 0),   # space for X row of text at ticks and to separate plots
    mgp = c(2.5, .7, 0),      #
    # pin = c(5, 1),       # current plot dimensions (width, heigth) in inch
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

axis(side = 1, at = c(1,10,20,30,40,50,60,70))
axis(side = 2, las = 2, at = seq(-1, 1, .25), labels = c("--1.00","--.75","--.50","--.25",".00", ".25", ".50", ".75", "1.00"))

polygon(c(data$rmse, rev(data$rmse)), c(data$lower, rev(data$upper)), col = "grey", border = FALSE)
abline(0, 0, lty = 5)
lines(data$rmse, data[, 3], lty = 1)

dev.off()
