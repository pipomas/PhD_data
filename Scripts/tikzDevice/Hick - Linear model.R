library(tikzDevice)

names(res)
x <- seq(-1,10,.01)

# expFunction <- function(x,intercept,slope){I(intercept*exp(slope*x))}
round(mean(res$Hinter), digits = 0)
round(mean(res$Hslope), digits=0)
round(mean(res$HrSquare), digits =2)

round(summary(res$Hinter), digits=0)
round(sd(res$Hinter), digits=0)

round(summary(res$Hslope), digits=0)
round(sd(res$Hslope), digits=0)

y <- mean(res$Hinter) + (mean(res$Hslope)*x)

std <- function(x) sd(x)/sqrt(length(x))



# plot --------------------------------------------------------------------
plot.new()
par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 4.5), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7,0),      # 
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(x, y, lty = 1, pch = "", type = "o",
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     # log = "x",
     xlim = c(0,3),
     ylim = c(200, 450),
     yaxt = "n",
     xaxt = "n",
     
     ylab = paste("Reaktionszeit (ms)"),
     xlab= "Bits ($\\log_{2}\\textnormal{n}$)",
     cex.lab=1.2,
     #cex.sub=3,
     col = "black",
     
     cex = 1,
     bty = "n",
     las = 1)

# axis(side = 1, at = c(1,10), las=1, cex.axis = 1.2, labels = FALSE)
axis(side = 1, at = c(0,1,2,3), labels = c("0", "1", "2", "2.58"), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(200,450,50), las=2, cex.axis = 1.2)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2))

points(0, mean(res$H0meanRT),   cex = 1.5, pch = 20)
points(1, mean(res$H1meanRT),   cex = 1.5, pch = 20)
points(2, mean(res$H2meanRT),   cex = 1.5, pch = 20)
points(3, mean(res$H258meanRT), cex = 1.5, pch = 20)

# hack: we draw arrows but with very special "arrowheads"
std <- function(x) sd(x)/sqrt(length(x))
arrows(0, mean(res$H0meanRT)-std(res$H0meanRT), 0, mean(res$H0meanRT)+std(res$H0meanRT), length=0.05, angle=90, code=3)
arrows(1, mean(res$H1meanRT)-std(res$H1meanRT), 1, mean(res$H1meanRT)+std(res$H1meanRT), length=0.05, angle=90, code=3)
arrows(2, mean(res$H2meanRT)-std(res$H2meanRT), 2, mean(res$H2meanRT)+std(res$H2meanRT), length=0.05, angle=90, code=3)
arrows(3, mean(res$H258meanRT)-std(res$H258meanRT), 3, mean(res$H258meanRT)+std(res$H258meanRT), length=0.05, angle=90, code=3)


# tikz --------------------------------------------------------------------

tikz("output/tikz/linmodel.tex")
plot.new()
par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4, 4.5, 0, 4.5), # space for one row of text at ticks and to separate plots
    mgp = c(3, .7,0),      # 
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(x, y, lty = 1, pch = "", type = "o",
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     # log = "x",
     xlim = c(0,3),
     ylim = c(200, 450),
     yaxt = "n",
     xaxt = "n",
     
     ylab = paste("Reaktionszeit (ms)"),
     # xlab= expression(paste("Bits (", log[2], " n)")),
     xlab= "Bits ($\\log_{2}\\textnormal{n}$)",
     cex.lab=1.2,
     #cex.sub=3,
     col = "black",
     
     cex = 1,
     bty = "n",
     las = 1)

# axis(side = 1, at = c(1,10), las=1, cex.axis = 1.2, labels = FALSE)
axis(side = 1, at = c(0,1,2,3), labels = c("0", "1", "2", "2.58"), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(200,450,50), las=2, cex.axis = 1.2)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2))

points(0, mean(res$H0meanRT),   cex = 1.5, pch = 20)
points(1, mean(res$H1meanRT),   cex = 1.5, pch = 20)
points(2, mean(res$H2meanRT),   cex = 1.5, pch = 20)
points(3, mean(res$H258meanRT), cex = 1.5, pch = 20)

# hack: we draw arrows but with very special "arrowheads"
std <- function(x) sd(x)/sqrt(length(x))
arrows(0, mean(res$H0meanRT)-std(res$H0meanRT), 0, mean(res$H0meanRT)+std(res$H0meanRT), length=0.05, angle=90, code=3)
arrows(1, mean(res$H1meanRT)-std(res$H1meanRT), 1, mean(res$H1meanRT)+std(res$H1meanRT), length=0.05, angle=90, code=3)
arrows(2, mean(res$H2meanRT)-std(res$H2meanRT), 2, mean(res$H2meanRT)+std(res$H2meanRT), length=0.05, angle=90, code=3)
arrows(3, mean(res$H258meanRT)-std(res$H258meanRT), 3, mean(res$H258meanRT)+std(res$H258meanRT), length=0.05, angle=90, code=3)




dev.off()