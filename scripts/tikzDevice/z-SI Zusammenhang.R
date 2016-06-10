library(tikzDevice)

# in R --------------------------------------------------------------------

par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    # mai = c(8, 4.5, 2, 5), # 4.5? space for one row of text at ticks and to separate plots
    mar = c(4.1, 4, 0, 0), # 4.5? space for one row of text at ticks and to separate plots
    mgp = c(1.8, .7),      # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)           # allow content to protrude into outer margin (and beyond))

plot(res$SI2, res$zTotal,
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     xlim = c(-.2,1),
     ylim = c(-2,2),
     yaxt = "n",
     xaxt = "n",
     ylab = "BIS-Test z-Wert",
     xlab = "Suppression-Index",
     cex.lab=1.2,
     #cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = seq(-.20,1,.20), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(-2,2,1), las=2, cex.axis = 1.2)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2)



# Generate .tex file ------------------------------------------------------
tikz("output/tikz/z-SI.tex")

par(pty="s",
    oma = c(0,0,0,0),      # two rows of text at the outer left and bottom margin
    mar = c(4.1, 4, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(1.8, .7),      # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)

plot(res$SI2, res$zTotal,
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     xlim = c(-.2,1),
     ylim = c(-2,2),
     yaxt = "n",
     xaxt = "n",
     ylab = "BIS-Test \\textit{z}-Wert",
     xlab = "Suppression-Index",
     cex.lab=1.2,
     #cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = seq(-.20,1,.20), las=1, cex.axis = 1.2)
axis(side = 2, at = seq(-2,2,1), las=2, cex.axis = 1.2)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2)



dev.off()
